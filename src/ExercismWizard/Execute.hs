{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module ExercismWizard.Execute
  ( findCli
  , ExercismCli (..)
  , execute
  , exerciseProjectHome
  , exerciseMetaDirExists
  )
where

import Control.Monad
import Data.Char
import Data.List (partition)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import ExercismWizard.CommandParse
import ExercismWizard.FSPath hiding (null)
import qualified Data.ByteString as BS
import ExercismWizard.Language
  ( Language (..)
  , getLanguage
  , langName
  , parseLangTrack
  , peekRepoUrl
  , peekSolutionUrl
  )
import ExercismWizard.Types
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath.Posix (pathSeparator)
import qualified System.IO
import System.Posix.Daemon (Redirection (..), runDetached)
import qualified System.Process as SP
import Turtle.Prelude
import Turtle.Shell
import Prelude hiding (FilePath)
import qualified ExercismWizard.Config.EditThisCookie as ETC

{-
  Find infomation on existing exercism cli setup.
  This is also to confirm that the binary is installed and configured.
 -}
findCli :: IO ExercismCli
findCli = do
  Just binPath <- which "exercism"
  (ExitSuccess, out) <- procStrict (toText binPath) ["workspace"] ""
  let [fromText -> workspace] = T.lines out
  True <- testdir workspace
  workspaceReal <- realpath workspace
  pure ExercismCli {binPath, workspace, workspaceReal}

exerciseProjectHome :: ExercismCli -> Exercise -> FilePath
exerciseProjectHome ExercismCli {workspace} Exercise {langTrack, name} =
  workspace </> fromText (langName langTrack) </> fromText name

exerciseMetaDirExists :: ExercismCli -> Exercise -> IO Bool
exerciseMetaDirExists cli e =
  testdir $ exerciseProjectHome cli e </> ".exercism"

guessExercise :: Bool -> ExercismCli -> IO (Maybe Exercise)
guessExercise checkMeta cli@ExercismCli {workspaceReal} = do
  cwd <- pwd >>= realpath
  let lePair = do
        {-
           stripPrefix has a rather unintuitive behavior:

           > stripPrefix "/a/b" "/a/b/c"
           Nothing
           > stripPrefix "/a/b/" "/a/b/c"
           Just (FilePath "c")

           This is due to stripPrefix splits a path into (directory,basename and extension) pair,
           while canonicalized path does not have the trailing path separator.
           The fix is to append an "" after the prefix path.
        -}
        xs <- stripPrefix (workspaceReal </> "") (cwd </> "")
        lPre : ePre : _ <- pure $ toText <$> splitDirectories xs
        (l, lSep) <- T.unsnoc lPre
        guard $ lSep == pathSeparator
        (e, eSep) <- T.unsnoc ePre
        guard $ eSep == pathSeparator
        pure (l, e)
  case lePair of
    Just (langTrackRaw, name) | Just langTrack <- parseLangTrack langTrackRaw -> do
      let exer = Exercise {langTrack, name}
      e <-
        if checkMeta
          then exerciseMetaDirExists cli exer
          else pure True
      pure $
        guard e >> Just exer
    _ -> pure Nothing

fillExercise :: Bool -> ExercismCli -> RawExercise -> IO Exercise
fillExercise checkMeta ec (RawExercise (l, e)) = case (l, e) of
  (Just l', Just e') -> pure (Exercise l' e')
  _ -> do
    guessed <- guessExercise checkMeta ec
    case guessed of
      Nothing -> do
        putStrLn "Cannot determine track or exericse."
        exitFailure
      Just Exercise {langTrack = gl, name = gn} ->
        pure $ Exercise (fromMaybe gl l) (fromMaybe gn e)

pprExercise :: Exercise -> IO ()
pprExercise Exercise {langTrack, name} =
  T.putStrLn $ T.pack (show langTrack) <> " track, exercise: " <> name

openUrlDetached :: T.Text -> IO ()
openUrlDetached url =
  runDetached Nothing DevNull (void runProg)
  where
    runProg = proc "xdg-open" [url] ""

execute :: ExercismCli -> Command -> IO ()
execute cli@ExercismCli {binPath} cmd = case cmd of
  CmdProxy args -> proc binPathT args "" >>= exitWith
  CmdLangAction actionTy rawExer extraArgs -> do
    e@Exercise {langTrack} <- fillExercise True cli rawExer
    pprExercise e
    case actions (getLanguage langTrack) M.!? actionTy of
      Just action -> do
        cd (exerciseProjectHome cli e)
        case action of
          RunProgram pg as detach ->
            let runProg = proc pg (as <> extraArgs) ""
             in if detach
                  then runDetached Nothing DevNull (void runProg)
                  else runProg >>= exitWith
          RunIO act -> act cli e extraArgs
      Nothing -> do
        putStrLn $ show actionTy <> " action not supported for this language."
        exitFailure
  CmdGet raw -> handleGetThen False raw (const (pure ()))
  CmdOn raw -> handleGetThen True raw $ \e -> do
    Just shBin <- need "SHELL"
    {-
      If the working directory of exercism is a soft link, simply
      chdir to it programatically will actual result in resolving the path
      to its realpath, which might be very long. This is usually fine,
      but since users usually have their current working directory printed
      somewhere in their PS1, this quickly becomes unpleasant to see.

      For shells that support "-i" and "-c", following approach is available:

      $SHELL -i -c "cd <dir>; $SHELL"

      By letting shell to chdir for itself, it can keep PWD clean while
      switching to the actual path.
     -}
    let exerProjectHomeAsStr = encodeString (exerciseProjectHome cli e)
        isSafeDirPath =
          -- this check is a bit over conservative, but it's better to be safe.
          all isSafeChar exerProjectHomeAsStr
          where
            isSafeChar c =
              isAlphaNum c || (c `elem` ("/-_." :: String))
        shName = toText (filename (fromText shBin))
        isScriptableShell = shName `elem` T.words "sh bash dash fish ksh mksh zsh"
        runCP cprocPre =
          let cproc =
                cprocPre
                  { SP.std_in = SP.Inherit
                  , SP.std_out = SP.Inherit
                  , SP.std_err = SP.Inherit
                  , SP.delegate_ctlc = True
                  }
           in system cproc "" >>= exitWith
        spawnThenChdirWithScript =
          runCP
            (SP.proc
               (T.unpack shBin)
               [ "-i"
               , "-c"
               , "cd "
                   <> exerProjectHomeAsStr
                   <> "; "
                   <> T.unpack shBin
               ])
        -- a general approach for spawning shells
        generalShellSpawn =
          runCP
            (SP.proc (T.unpack shBin) []) {SP.cwd = Just exerProjectHomeAsStr}
    if isScriptableShell && isSafeDirPath
      then spawnThenChdirWithScript
      else generalShellSpawn
  CmdEdit raw -> handleGetThen True raw $ \e@Exercise {langTrack} -> do
    let prjHome = exerciseProjectHome cli e
        Language {solutionFiles, editMethod} = getLanguage langTrack
    sh $ do
      pushd prjHome
      case editMethod of
        Nothing -> liftIO $ do
          putStrLn "Edit is not supported for this language"
          exitFailure
        Just OpenWithEditor -> do
          files <- liftIO $ solutionFiles e
          case files of
            [] -> liftIO $ do
              putStrLn "No files to edit."
              exitFailure
            hd : _tl ->
              liftIO $ do
                Just editorCmd <- need "EDITOR"
                runDetached Nothing DevNull $ void (proc editorCmd [toText hd] "")
        Just (OpenProjectWithProgram prog) ->
          liftIO $
            runDetached Nothing DevNull $ void (proc prog ["."] "")
  CmdRemoveIgnore raw -> do
    e@Exercise {langTrack} <- fillExercise True cli raw
    let Language {removeIgnore} = getLanguage langTrack
    pprExercise e
    case removeIgnore of
      Nothing -> do
        putStrLn "rmignore not supported by this language."
      Just (searchPath, pat, ignoreLineMarker) -> sh $ do
        pushd (exerciseProjectHome cli e)
        fp <- find pat searchPath
        liftIO $ do
          let sFp = encodeString fp
          origContent <- T.readFile sFp
          let xs = T.lines origContent
              (remove, newXs) = partition ((== ignoreLineMarker) . T.strip) xs
          T.writeFile sFp (T.unlines newXs)
          T.putStrLn $
            "Removed " <> T.pack (show (length remove)) <> " lines from " <> T.pack sFp
  CmdSubmit raw -> do
    e@Exercise {langTrack} <- fillExercise True cli raw
    let Language {solutionFiles} = getLanguage langTrack
    pprExercise e
    {-
      Special care is required as this is the only command that
      indirectly uploads files to Internet.

      checks:
      - file set is non-empty.
      - all file paths are relative.
      - all files must exist.
      - require user confirmation.
     -}
    sh $ do
      pushd (exerciseProjectHome cli e)
      submitFps <- liftIO $ solutionFiles e
      when (null submitFps) $ die "No file to submit."
      forM_ submitFps $ \fp -> do
        unless (relative fp) $
          die $ "File " <> toText fp <> " is not relative."
        exist <- testfile fp
        unless exist $
          die $ "File " <> toText fp <> " does not exist."
      cwd <- pwd
      let submitFpTs = fmap toText submitFps
      liftIO $ do
        T.putStrLn $ "Current working directory: " <> toText cwd
        T.putStrLn $ "Files to be submitted: " <> T.intercalate ", " submitFpTs
        T.putStr "y to confirm: "
        System.IO.hFlush System.IO.stdout
        s <- getLine
        unless (s `elem` ["Y", "y"]) $
          die "Submission aborted"
        proc binPathT ("submit" : submitFpTs) "" >>= exitWith
  CmdPeekLangRepo lt -> openUrlDetached (peekRepoUrl lt)
  CmdPeekSolution raw -> do
    e <- fillExercise True cli raw
    openUrlDetached (peekSolutionUrl e)
  CmdDebug _args ->
    sh $ do
      let ExercismCli {workspace} = cli
      langPath <- ls workspace
      let lang = toText $ filename langPath
      guard $ not $ T.isPrefixOf "." lang
      case parseLangTrack lang of
        Nothing -> pure ()
        Just langTrack | langTrack /= Haskell -> pure ()
        Just langTrack -> do
          let l = getLanguage langTrack
          liftIO $ putStrLn $ "Track: " <> show langTrack
          prjHome <- ls langPath
          let eName = toText (filename prjHome)
          guard $ not $ T.isPrefixOf "." eName
          liftIO $ T.putStrLn $ "  Exercise: " <> eName
          pushd prjHome
          liftIO $ do
            xs <- solutionFiles l (Exercise langTrack eName)
            T.putStrLn $ "    Files: " <> T.intercalate ", " (fmap toText xs)
  CmdSaveCookie -> do
    putStrLn "Reading for stdin for EditThisCookie JSON export ..."
    raw <- BS.getContents
    print (ETC.decodeCookies @T.Text raw)
  where
    binPathT = toText binPath
    handleGetThen quiet raw action = do
      e@Exercise {langTrack, name} <- fillExercise False cli raw
      pprExercise e
      alreadyExist <- exerciseMetaDirExists cli e
      if alreadyExist
        then do
          unless quiet $
            putStrLn "Metadata already exist, skipping."
          action e
        else do
          ec <-
            proc
              binPathT
              [ "download"
              , "--exercise=" <> name
              , "--track=" <> langName langTrack
              ]
              ""
          if ec == ExitSuccess
            then action e
            else exitWith ec

{-
  _ ->
    do
      print cli
      putStrLn "Not yet supported:"
      print cmd
      exitFailure
-}
