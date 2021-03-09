{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ExercismWizard.Language.Haskell
  ( runOrmolu
  , findSolutionFiles
  )
where

import qualified Control.Foldl as Fold
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import ExercismWizard.FSPath hiding (null)
import ExercismWizard.Types
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell
import Prelude hiding (FilePath)

runOrmolu :: ExercismCli -> Exercise -> [T.Text] -> IO CmdSpec
runOrmolu _ _ _ = do
  cwd <- pwd
  srcFiles <-
    reduce
      Fold.mconcat
      (forM
         ["src", "test"]
         (\s ->
            find (suffix ".hs") (cwd </> s)))
  putStrLn $ "Collected " <> show (length srcFiles) <> " file(s) for formatting."
  pure $ CmdSpec "ormolu" ("--mode=inplace" : fmap toText srcFiles) False

findSolutionFiles :: Exercise -> IO [FilePath]
findSolutionFiles _e = do
  -- TODO: package.yaml should be conditionally added.
  xs <- reduce Fold.list (find (suffix ".hs") "src/")
  packageYaml <- Yaml.decodeFileThrow @_ @Yaml.Value "package.yaml"
  let depList :: Maybe Yaml.Array
      depList = do
        Yaml.Object infoRoot <- pure packageYaml
        Yaml.Object lib <- HM.lookup "library" infoRoot
        Yaml.Array libDep <- HM.lookup "dependencies" lib
        pure libDep
  let mayPackageYaml = [fromText "package.yaml" | not (null depList)]
  pure (xs <> mayPackageYaml)
