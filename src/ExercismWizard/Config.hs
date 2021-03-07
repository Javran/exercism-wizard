{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module ExercismWizard.Config where

import Dhall

data Config = Config deriving (Generic, FromDhall)
