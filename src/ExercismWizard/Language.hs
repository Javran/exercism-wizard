{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module ExercismWizard.Language
  ( LangTrack (..)
  , Language (..)
  , Action (..)
  , go
  , kotlin
  , rust
  , haskell
  , parseLangTrack
  , langName
  , getLanguage
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

data LangTrack
  = Haskell
  | Kotlin
  | Rust
  | Go
  deriving (Show, Eq, Ord)

data Action
  = Format
  | Test
  | Lint
  deriving (Eq, Ord, Show)

data Language = Language
  { track :: LangTrack
  , altNames :: [T.Text]
  , actions :: M.Map Action T.Text -- TODO: Text can be further refined to proc / shell and whether fork() is necessary.
  }

langName :: LangTrack -> T.Text
langName = T.toLower . T.pack . show

-- TODO: peekRepo seems common: "https://github.com/exercism/<lang>/tree/main/exercises/practice"

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

go :: Language
go =
  Language
    { track = Go
    , altNames = []
    , actions =
        M.fromList $
          [ (Format, "go fmt")
          , (Test, "go test -v --bench . --benchmem")
          , (Lint, "golint") -- TODO: this linter might need a list of files.
          ]
    }

kotlin :: Language
kotlin =
  Language
    { track = Kotlin
    , altNames = ["kt"]
    , actions = M.empty
    }

rust :: Language
rust =
  Language
    { track = Rust
    , altNames = ["rs"]
    , actions =
        M.fromList
          [ (Format, "cargo fmt")
          , (Test, "cargo test")
          , (Lint, "cargo clippy --all-targets")
          ]
    }

haskell :: Language
haskell =
  Language
    { track = Haskell
    , altNames = ["hs"]
    , actions =
        M.fromList
          [ (Test, "stack test")
          ]
    }
