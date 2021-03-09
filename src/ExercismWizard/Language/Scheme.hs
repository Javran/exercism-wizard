{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ExercismWizard.Language.Scheme
  ( runTests
  )
where

import Control.Applicative
import Control.Monad
import qualified Data.Text as T
import ExercismWizard.FSPath
import ExercismWizard.Types
import Turtle.Prelude

{-
  Scheme track attempts to be RnRS-compliant but for now there are still
  make exercises that works only for Guile but not ChezScheme.

  So instead of calling `make` when `MAKEFILE` is present, we just call `guile`
  with appropriate test file direction.

  However for now there is some consistency issue on how the test file is named:

  - list-ops -> list-ops-test.scm
  - robot-name -> robot-name-test.scm
  - other than those above, test.scm

  In summary there's either:

  - test.scm, used by most of the exercises
  - or <exercise-name>-test.scm, used by list-ops and robot-name.

  The following is a workaround based on this to make `ew test` work regardless of the situation.

 -}
runTests :: ExercismCli -> Exercise -> [T.Text] -> IO CmdSpec
runTests _cli Exercise {name} _extraArgs = do
  testScm <- do
    let validFile fp = testfile fp >>= \exist -> guard exist >> pure fp
    validFile "test.scm"
      <|> validFile (fromText (name <> toText "-test.scm"))
      <|> fail "Cannot find any test file."
  pure $ CmdSpec "guile" [toText testScm] False
