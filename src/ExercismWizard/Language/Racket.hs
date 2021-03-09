{-# LANGUAGE OverloadedStrings #-}

module ExercismWizard.Language.Racket
  ( runTests
  )
where

import qualified Control.Foldl as Fold
import qualified Data.Text as T
import ExercismWizard.FSPath
import ExercismWizard.Types
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell

runTests :: ExercismCli -> Exercise -> [T.Text] -> IO CmdSpec
runTests _cli _e _extraArgs = do
  cwd <- pwd
  testFiles <- fmap toText <$> reduce Fold.list (find (suffix "test.rkt") cwd)
  pure $ CmdSpec "raco" ("test" : testFiles) False
