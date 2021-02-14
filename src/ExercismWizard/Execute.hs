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
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import ExercismWizard.CommandParse
import ExercismWizard.FSPath
import ExercismWizard.Language
  ( Language (..)
  , getLanguage
  , langName
  , parseLangTrack
  )
import ExercismWizard.Types
import System.Exit
import System.FilePath.Posix (pathSeparator)
import qualified System.Process as SP
import Turtle.Prelude
import Prelude hiding (FilePath)

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
        lPre : ePre : _ <- pure $ fmap toText $ splitDirectories xs
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

execute :: ExercismCli -> Command -> IO ()
execute cli@ExercismCli {binPath} cmd = case cmd of
  CmdProxy args -> proc binPathT args "" >>= exitWith
  CmdLangAction actionTy rawExer -> do
    e@Exercise {langTrack} <- fillExercise True cli rawExer
    pprExercise e
    case actions (getLanguage langTrack) M.!? actionTy of
      Just action -> do
        cd (exerciseProjectHome cli e)
        case action of
          RunProgram pg as ->
            proc pg as "" >>= exitWith
          RunIO act -> act cli e
      Nothing -> do
        putStrLn $ show actionTy <> " action not supported for this language."
        exitFailure
  CmdGet raw -> handleGetThen False raw (const (pure ()))
  CmdOn raw -> handleGetThen True raw $ \e -> do
    Just shBin <- need "SHELL"
    let cproc =
          (SP.proc (T.unpack shBin) [])
            { SP.std_in = SP.Inherit
            , SP.std_out = SP.Inherit
            , SP.std_err = SP.Inherit
            , SP.delegate_ctlc = True
            , SP.cwd = Just (encodeString (exerciseProjectHome cli e))
            }
    system cproc "" >>= exitWith
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
