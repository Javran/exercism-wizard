{-# LANGUAGE OverloadedStrings #-}

module ExercismWizard.Language.Haskell
  ( runOrmolu
  )
where

import Control.Monad
import ExercismWizard.FSPath
import ExercismWizard.Types
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell

runOrmolu :: ExercismCli -> Exercise -> IO ()
runOrmolu _ _ = sh $ do
  cwd <- pwd
  forM_ ["src", "test"] $ \s -> do
    fn <- find (suffix ".hs") (cwd </> s)
    liftIO $ putStrLn $ "Formating: " <> show (toText fn)
    _ <- proc "ormolu" ["--mode=inplace", toText fn] ""
    pure ()
