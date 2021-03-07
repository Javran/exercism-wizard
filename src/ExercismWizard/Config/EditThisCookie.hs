{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

{-
  This module supports JSON format exported from
  EditThisCookie extension by http://www.editthiscookie.com
 -}
module ExercismWizard.Config.EditThisCookie
  ( Cookie (..)
  , decodeCookies
  )
where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Scientific
import Data.Time.Calendar
import Data.Time.Clock
import GHC.Generics

newtype TimeConv = TimeConv UTCTime deriving (Show) via UTCTime

instance FromJSON TimeConv where
  parseJSON = withScientific "time" $ \t -> do
    let dt = realToFrac $ toRealFloat @Double t
        epoch = UTCTime {utctDay = fromGregorian 1970 1 1, utctDayTime = 0}
    pure $ TimeConv $ addUTCTime dt epoch

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
  , expirationDate :: Maybe TimeConv
  , storeId :: text
  }
  deriving (Generic, FromJSON, Functor, Show)

decodeCookies :: FromJSON text => BS.ByteString -> Either String [Cookie text]
decodeCookies = eitherDecodeStrict'
