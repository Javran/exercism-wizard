{-# LANGUAGE OverloadedStrings #-}
module ExercismWizard.Language.Haskell
   (
 runOrmolu
   ) where

import ExercismWizard.Types
import Turtle.Prelude
import Turtle.Pattern
import Turtle.Shell
import ExercismWizard.FSPath

runOrmolu :: ExercismCli -> Exercise -> IO ()
runOrmolu _ _ = sh $ do
  fn <- find (suffix ".hs") =<< pwd
  liftIO $ putStrLn $ "Formating: " <> show (toText fn)
  _ <- proc "ormolu" ["--mode=inplace", toText fn] ""
  pure ()
