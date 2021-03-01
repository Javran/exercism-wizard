{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module ExercismWizard.Language
  ( LangTrack (..)
  , Language (..)
  , parseLangTrack
  , langName
  , getLanguage
  , peekRepoUrl
  , peekSolutionUrl
  )
where

import qualified Control.Foldl as Fold
import Data.Functor
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import ExercismWizard.FSPath
import ExercismWizard.Language.Haskell as LangHaskell
  ( findSolutionFiles
  , runOrmolu
  )
import ExercismWizard.Types
  ( Action (..)
  , ActionType (..)
  , EditMethod (..)
  , Exercise (..)
  , LangTrack (..)
  )
import Turtle.Pattern
import Turtle.Prelude hiding (err)
import Turtle.Shell
import Prelude hiding (FilePath)

data Language = Language
  { track :: LangTrack
  , altNames :: [T.Text]
  , -- | Note that actions can assume current directory is the project root of that exercise.
    actions :: M.Map ActionType Action
  , -- | Return a list of files considered part of a solution to be submitted.
    --   note that this is kept as a list rather than a set as the first file is
    --   usually also the file to be opened by edit subcommand.
    solutionFiles :: Exercise -> IO [FilePath]
  , editMethod :: Maybe EditMethod
  , -- | When set: (testpath, pattern, <line>), meaning scan all files under testpath
    --   and remove lines that contains only <line> and surrounding whitespaces.
    --   TODO: to be implemented.
    removeIgnore :: Maybe (FilePath, Pattern (), T.Text)
  }

langName :: LangTrack -> T.Text
langName = T.toLower . T.pack . show

peekRepoUrl :: LangTrack -> T.Text
peekRepoUrl lt =
  "https://github.com/exercism/"
    <> langName lt
    <> "/tree/main/exercises/practice"

peekSolutionUrl :: Exercise -> T.Text
peekSolutionUrl Exercise {langTrack, name} =
  "https://exercism.io/tracks/" <> langName langTrack
    <> "/exercises/"
    <> name
    <> "/solutions/"

-- TODO: do we have a better way of collecting all defined languages?
languages :: [Language]
languages = [haskell, kotlin, rust, go, scheme]

langTracks :: M.Map T.Text LangTrack
langTracks = M.fromListWith err $ do
  Language {track, altNames} <- languages
  (,track) <$> langName track : altNames
  where
    err = error "Conflicting keys"

parseLangTrack :: T.Text -> Maybe LangTrack
parseLangTrack = (langTracks M.!?)

getLanguage :: LangTrack -> Language
getLanguage lt = head $ filter ((== lt) . track) languages

rp :: T.Text -> Action
rp xs = RunProgram y ys False
  where
    y : ys = T.words xs

findThenIgnoreTests :: Pattern a -> Pattern b -> IO [FilePath]
findThenIgnoreTests srcExt testSuffix =
  reduce
    Fold.list
    (do
       fp <- find (suffix srcExt) "."
       let fp' = toText fp
       [] <- pure $ match (suffix testSuffix) fp'
       pure fp)

go :: Language
go =
  Language
    { track = Go
    , altNames = []
    , actions =
        M.fromList
          [ (Format, rp "go fmt")
          , (Test, rp "go test -v --bench . --benchmem")
          , (Lint, rp "golint")
          ]
    , solutionFiles = const $ findThenIgnoreTests ".go" "_test.go"
    , editMethod = Just OpenWithEditor
    , removeIgnore = Nothing
    }

kotlin :: Language
kotlin =
  Language
    { track = Kotlin
    , altNames = ["kt"]
    , actions = M.empty
    , solutionFiles = \_e ->
        reduce Fold.list (find (suffix ".kt") "src/main")
    , editMethod = Just $ OpenProjectWithProgram "idea"
    , removeIgnore = Just ("src/test", void (suffix ".kt"), "@Ignore")
    }

rust :: Language
rust =
  Language
    { track = Rust
    , altNames = ["rs"]
    , actions =
        M.fromList
          [ (Format, rp "cargo fmt")
          , (Test, rp "cargo test")
          , (Lint, rp "cargo clippy --all-targets")
          ]
    , solutionFiles = \_e -> do
        let src = "src/lib.rs"
        b <- testfile src
        pure [src | b]
    , editMethod = Just OpenWithEditor
    , removeIgnore = Just ("tests", void (suffix ".rs"), "#[ignore]")
    }

haskell :: Language
haskell =
  Language
    { track = Haskell
    , altNames = ["hs"]
    , actions =
        M.fromList
          [ (Format, RunIO LangHaskell.runOrmolu)
          , (Test, rp "stack test")
          , (Lint, rp "hlint .")
          ]
    , solutionFiles = LangHaskell.findSolutionFiles
    , editMethod = Just OpenWithEditor
    , removeIgnore = Nothing
    }

scheme :: Language
scheme =
  Language
    { track = Scheme
    , altNames = ["scm"]
    , actions = M.singleton Test (rp "make guile")
    , solutionFiles =const $ findThenIgnoreTests ".scm" "test.scm"
    , editMethod = Just OpenWithEditor
    , removeIgnore = Nothing
    }
