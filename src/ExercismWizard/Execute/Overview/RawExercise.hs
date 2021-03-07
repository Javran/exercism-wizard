{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ExercismWizard.Execute.Overview.RawExercise
  ( RawExercise (..)
  )
where

import Data.Aeson
import GHC.Generics

data RawExercise = RawExercise
  { status :: String
  , uri :: Maybe String
  , name :: String
  , eId :: String
  , core :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
