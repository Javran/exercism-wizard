{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ExercismWizard.Config where

import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import Dhall
import Dhall.Core
import System.Directory
import System.FilePath.Posix

data Config = Config
  { userCookies :: M.Map Text Text
  }
  deriving (Generic, FromDhall, ToDhall)

configPath :: IO FilePath
configPath = do
  dir <- getXdgDirectory XdgConfig "exercism-wizard"
  createDirectoryIfMissing True dir
  pure $ dir </> "config.dhall"

-- TODO: how do we make sure not to overwrite a newer file (since that start of execution)
writeConfig :: Config -> IO ()
writeConfig c = do
  fp <- configPath
  T.writeFile fp (pretty $ embed inject c)
