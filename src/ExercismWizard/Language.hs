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

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import ExercismWizard.Types
  ( Action (..)
  , ActionType (..)
  , LangTrack (..)
  )

data Language = Language
  { track :: LangTrack
  , altNames :: [T.Text]
  , actions :: M.Map ActionType Action
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

rp :: T.Text -> Action
rp xs = RunProgram y ys
  where
    y:ys = T.words xs

go :: Language
go =
  Language
    { track = Go
    , altNames = []
    , actions =
        M.fromList $
          [ (Format, rp "go fmt")
          , (Test, rp "go test -v --bench . --benchmem")
          , (Lint, rp "golint") -- TODO: this linter might need a list of files.
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
          [ (Format, rp "cargo fmt")
          , (Test, rp "cargo test")
          , (Lint, rp "cargo clippy --all-targets")
          ]
    }

haskell :: Language
haskell =
  Language
    { track = Haskell
    , altNames = ["hs"]
    , actions =
        M.fromList
          [ (Test, rp "stack test")
          ]
    }
