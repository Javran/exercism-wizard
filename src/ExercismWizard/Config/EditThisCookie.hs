{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

{-
  This module supports JSON format exported from
  EditThisCookie extension by http://www.editthiscookie.com
 -}
module ExercismWizard.Config.EditThisCookie
  ( Cookie (..)
  , decodeCookies
  )
where

import Data.Scientific
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Time.Clock
import GHC.Generics

{- ref: http://www.editthiscookie.com/blog/2014/03/cookie-properties/ -}
data Cookie text = Cookie
  { name :: text
  , value :: text
  , domain :: text
  , hostOnly :: Bool
  , path :: text
  , secure :: Bool
  , httpOnly :: Bool
  , session :: Bool
  , expirationDate :: Maybe Scientific
  , storeId :: text
  }
  deriving (Generic, FromJSON, Functor, Show)

decodeCookies :: FromJSON text => BS.ByteString -> Either String [Cookie text]
decodeCookies = eitherDecodeStrict'
