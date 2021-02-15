module ExercismWizard.Types
  ( Exercise (..)
  , LangTrack (..)
  , ActionType (..)
  , Action (..)
  , ExercismCli (..)
  )
where

import qualified Data.Text as T
import ExercismWizard.FSPath
import Prelude hiding (FilePath)

data ExercismCli = ExercismCli
  { -- | Binary path to exercism CLI
    binPath :: FilePath
  , -- | Path to exercism workspace
    workspace :: FilePath
  , -- | Canonicalized path to exercism workspace
    workspaceReal :: FilePath
  }
  deriving (Show)

data Exercise = Exercise
  { langTrack :: LangTrack
  , name :: T.Text
  }
  deriving (Show)

data LangTrack
  = Haskell
  | Kotlin
  | Rust
  | Go
  deriving (Show, Eq, Ord)

data ActionType
  = Format
  | Test
  | Lint
  deriving (Eq, Ord, Show)

data Action
  = RunProgram
      { progName :: T.Text
      , progArgs :: [T.Text]
      }
  | RunIO (ExercismCli -> Exercise -> [T.Text] -> IO ())
