{-# LANGUAGE OverloadedStrings #-}

module ExercismWizard.Language.Haskell
  ( runOrmolu
  )
where

import qualified Control.Foldl as Foldl
import Control.Monad
import Data.Monoid
import qualified Data.Text as T
import ExercismWizard.FSPath
import ExercismWizard.Types
import System.Exit
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell

runOrmolu :: ExercismCli -> Exercise -> [T.Text] -> IO ()
runOrmolu _ _ _ = do
  t <-
    getSum
      <$> (reduce (Foldl.mconcat) $ do
             cwd <- pwd
             mconcat
               <$> (forM ["src", "test"] $ \s -> do
                      fn <- find (suffix ".hs") (cwd </> s)
                      ec <- proc "ormolu" ["--mode=inplace", toText fn] ""
                      if ec == ExitSuccess
                        then pure (1 :: Sum Int)
                        else do
                          liftIO $
                            putStrLn $
                              "Failure when formating " <> T.unpack (toText fn) <> ", " <> show ec
                          pure mempty))
  putStrLn $ "Formatted " <> show t <> " file(s)."
