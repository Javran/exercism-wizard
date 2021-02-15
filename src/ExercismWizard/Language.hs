{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module ExercismWizard.Language
  ( LangTrack (..)
  , Language (..)
  , go
  , kotlin
  , rust
  , haskell
  , parseLangTrack
  , langName
  , getLanguage
  )
where

import qualified Control.Foldl as Fold
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import ExercismWizard.FSPath
import ExercismWizard.Language.Haskell (runOrmolu)
import ExercismWizard.Types
  ( Action (..)
  , ActionType (..)
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
  }

langName :: LangTrack -> T.Text
langName = T.toLower . T.pack . show

{-

  TODO: some URL patterns:

  - peekRepo: "https://github.com/exercism/<lang>/tree/main/exercises/practice"
  - peekSol: "https://exercism.io/tracks/<lang>/exercises/<exercise>/solutions/"

 -}
languages :: [Language]
languages = [haskell, kotlin, rust, go]

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
rp xs = RunProgram y ys
  where
    y : ys = T.words xs

go :: Language
go =
  Language
    { track = Go
    , altNames = []
    , actions =
        M.fromList $
          [ (Format, rp "go fmt")
          , (Test, rp "go test -v --bench . --benchmem")
          , (Lint, rp "golint")
          ]
    , solutionFiles = \_e -> do
        reduce
          Fold.list
          (do
             fp <- find (suffix ".go") "."
             let fp' = toText fp
             [] <- pure $ match (suffix "_test.go") fp'
             pure fp)
    }

kotlin :: Language
kotlin =
  Language
    { track = Kotlin
    , altNames = ["kt"]
    , actions = M.empty
    , solutionFiles = \_e ->
        reduce Fold.list (find (suffix ".kt") "src/main")
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
    }

haskell :: Language
haskell =
  Language
    { track = Haskell
    , altNames = ["hs"]
    , actions =
        M.fromList
          [ (Format, RunIO runOrmolu)
          , (Test, rp "stack test")
          , (Lint, rp "hlint .")
          ]
    , solutionFiles = \_e ->
        reduce Fold.list (find (suffix ".hs") "src/")
    }
