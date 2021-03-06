module ExercismWizard.Types
  ( Exercise (..)
  , LangTrack (..)
  , ActionType (..)
  , Action (..)
  , CmdSpec (..)
  , ExercismCli (..)
  , EditMethod (..)
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
  | Scheme
  | Racket
  deriving (Show, Eq, Ord)

data ActionType
  = Format
  | Test
  | Lint
  deriving (Eq, Ord, Show)

data CmdSpec = CmdSpec
  { progName :: T.Text
  , progArgs :: [T.Text]
  , -- | whether to detach the process from current one
    detach :: Bool
  }

-- Action to be taken for a exercise project.
data Action
  = -- | Run a program with arguments
    RunProgram CmdSpec
  | -- | Compute the program and arguments to run and then run it
    ComputeAndRun (ExercismCli -> Exercise -> [T.Text] -> IO CmdSpec)
  | -- | A general IO action, note that user can't have a verbose output
    --   on the program and arguments if this one is used.
    RunIO
      ( ExercismCli
        -> Exercise
        -> {- this one is the extra args passed from command line -} [T.Text]
        -> IO ()
      )

-- While we can support a general IO action, I want to see if there's some pattern that
-- allows us to put things into ADTs.
data EditMethod
  = -- | Open just the first of the solution file with EDITOR
    OpenWithEditor
  | -- | Open with an external program with solution's directory passed (IntelliJ IDEA special)
    OpenProjectWithProgram T.Text
  | -- | Collect all source files and open them with an external program (DrRacket)
    OpenAllFilesWithProgram T.Text
