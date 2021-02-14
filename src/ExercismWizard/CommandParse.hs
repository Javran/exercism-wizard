{-# LANGUAGE OverloadedStrings #-}

module ExercismWizard.CommandParse
  ( getArgs
  , Command (..)
  , RawExercise (..)
  )
where

import Data.List.Split (splitOn)
import qualified Data.Text as T
import ExercismWizard.Language (Action (..), LangTrack, parseLangTrack)
import Options.Applicative
import Options.Applicative.Types
import qualified System.Environment as Env

data Command
  = CmdProxy [T.Text]
  | CmdLangAction Action RawExercise
  deriving (Show)

newtype RawExercise
  = RawExercise
      ( Maybe LangTrack -- language name
      , Maybe T.Text -- exercise name
      )
  deriving (Show)

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
       )
       <**> helper)
    (fullDesc
       <> header "Exercism Wizard - exercism workflow automation")
  where
    proxyCommand =
      command
        "proxy"
        (info
           (error "CmdProxy should not be reachable from within optparse-applicative framework.")
           (progDesc "Proxy all following arguments to exercism cli."))
    langActionCommand act cmdStr cmdDesc =
      command
        cmdStr
        (info
           (CmdLangAction act
              <$> argument
                rawExercise
                (help
                   "Specifies language track and exercise name. \
                   \Format: `[lang]:[exercise-name]`, left any part blank \
                   \to guess from current directory."
                   <> value (RawExercise (Nothing, Nothing))
                   <> metavar "EXERCISE")
              <**> helper)
           (fullDesc <> progDesc cmdDesc))

parseArgs :: [String] -> ParserResult Command
parseArgs xs = case xs of
  "proxy" : args ->
    {-
      proxy subcommand is a special mode that passes everything after it as-is
     -}
    Success $ CmdProxy $ fmap T.pack args
  _ -> execParserPure defaultPrefs opts xs

getArgs :: IO Command
getArgs = parseArgs <$> Env.getArgs >>= handleParseResult
