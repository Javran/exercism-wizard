{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ExercismWizard.CommandParse
  ( getArgs
  , Command (..)
  , RawExercise (..)
  , parseArgs
  )
where

import Data.List.Split (splitOn)
import qualified Data.Text as T
import ExercismWizard.Language (parseLangTrack)
import ExercismWizard.Types
import Options.Applicative
import Options.Applicative.Types
import qualified System.Environment as Env
import System.Exit

data Command
  = CmdProxy [T.Text]
  | CmdLangAction ActionType RawExercise [T.Text]
  | CmdGet RawExercise
  | CmdOn RawExercise
  | CmdEdit RawExercise
  | CmdRemoveIgnore RawExercise
  | CmdDebug [String]
  deriving (Show, Eq)

newtype RawExercise
  = RawExercise
      ( Maybe LangTrack -- language name
      , Maybe T.Text -- exercise name
      )
  deriving (Show, Eq)

{-
  Parses a raw description of an exercise, allowed formats:

  - "[<language>]:[<exercise>]"
  - "<exercise>"
  - ""

  Missing parts will be implied from current working directory,
  which is not handled by command line parser.

 -}
rawExercise :: ReadM RawExercise
rawExercise = do
  xs <- readerAsk
  RawExercise <$> case fmap T.pack $ splitOn ":" xs of
    ["", ""] -> pure (Nothing, Nothing)
    ["", e] -> pure (Nothing, Just e)
    [l, ""] | Just l' <- parseLangTrack l -> do
      pure (Just l', Nothing)
    [e] -> pure (Nothing, Just e)
    [l, e] | Just l' <- parseLangTrack l -> pure (Just l', Just e)
    _ -> readerError "Invalid RawExercise format."

opts :: ParserInfo Command
opts =
  info
    (subparser
       (proxyCommand
          <> langActionCommand Format "fmt" "Format source code."
          <> langActionCommand Test "test" "Run test suite."
          <> langActionCommand Lint "lint" "Run Linter."
          <> getCommand
          <> onCommand
          <> editCommand
          <> rmIgnoreCommand)
       <**> helper)
    (fullDesc
       <> header "Exercism Wizard - exercism workflow automation")
  where
    proxyCommand =
      command
        "proxy"
        (info
           (error
              "CmdProxy should not be reachable \
              \from within optparse-applicative framework.")
           (progDesc "Proxy all following arguments to exercism cli."))
    exerciseArg =
      argument
        rawExercise
        (help
           "Specifies language track and exercise name. \
           \Format: `[lang]:[exercise-name]`, left any part blank \
           \to guess from current directory."
           <> value (RawExercise (Nothing, Nothing))
           <> metavar "EXERCISE")
    extraArg =
      many
        (argument
           auto
           (help
              "Extra args passed to the custom command. \
              \e.g. ew <test|lint|fmt> -- --args0 --args1"
              <> metavar "ARGS..."))
    langActionCommand act cmdStr cmdDesc =
      command
        cmdStr
        (info
           (CmdLangAction act
              <$> exerciseArg
              <*> extraArg
              <**> helper)
           (fullDesc <> progDesc cmdDesc <> noIntersperse))
    getCommand =
      command
        "get"
        (info
           (CmdGet <$> exerciseArg <**> helper)
           (progDesc "`exercism download`, but avoid overwriting and less typing."))
    onCommand =
      command
        "on"
        (info
           (CmdOn <$> exerciseArg <**> helper)
           (progDesc "Spawn a sub-shell with exercise's project home."))
    editCommand =
      command
        "edit"
        (info
           (CmdEdit <$> exerciseArg <**> helper)
           (progDesc "Spawn an editor on specified exercise."))

    rmIgnoreCommand =
      command
        "rmignore"
        (info
           (CmdRemoveIgnore <$> exerciseArg <**> helper)
           (progDesc "Remove 'ignored' test annotations."))

{-
  Argument parsing results:
  - Right ParserResult: regular optparse-applicative result
  - Left [Text]: some extra arguments that cannot be attached to the current command.
 -}
parseArgs :: [String] -> Either [T.Text] (ParserResult Command)
parseArgs allArgs = case allArgs of
  "@debug": xs ->
    Right $ Success $ CmdDebug xs
  "proxy" : passArgs ->
    {-
      proxy subcommand is a special mode that passes everything after it as-is
     -}
    pure $ Success $ CmdProxy $ fmap T.pack passArgs
  _ ->
    let (args, mayExtraArgs) = span (/= "--") allArgs
        extraArgs = case mayExtraArgs of
          "--" : es -> Just es
          [] -> Nothing
          _ -> error "unreachable"
        parsed = execParserPure defaultPrefs opts args
        attachExtras :: [T.Text] -> Command -> Maybe Command
        attachExtras es = \case
          CmdLangAction aTy re _e -> Just $ CmdLangAction aTy re es
          _ -> Nothing
     in case extraArgs of
          Nothing -> Right parsed
          Just es | es' <- fmap T.pack es ->
            case parsed of
              Success cmd ->
                case attachExtras es' cmd of
                  Just cmd' -> Right (Success cmd')
                  Nothing -> Left es'
              _ -> Right parsed

getArgs :: IO Command
getArgs = do
  args <- Env.getArgs
  case parseArgs args of
    Right z -> handleParseResult z
    Left es -> do
      putStrLn $ "Extra arguments not supported by this subcommand: " <> show es
      exitFailure
