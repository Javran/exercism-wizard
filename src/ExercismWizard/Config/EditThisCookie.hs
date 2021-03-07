{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-
  This module supports JSON format exported from
  EditThisCookie extension by http://www.editthiscookie.com
 -}
module ExercismWizard.Config.EditThisCookie
  ( Cookie (..)
  , decodeCookies
  , toUserCookies
  )
where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Generics

newtype SecondSinceEpoch = SecondSinceEpoch UTCTime
  deriving (Show) via UTCTime

instance FromJSON SecondSinceEpoch where
  parseJSON =
    fmap (SecondSinceEpoch . posixSecondsToUTCTime)
      . parseJSON @POSIXTime

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
  , expirationDate :: Maybe SecondSinceEpoch
  , storeId :: text
  }
  deriving (Generic, FromJSON, Functor, Show)

decodeCookies :: FromJSON text => BS.ByteString -> Either String [Cookie text]
decodeCookies = eitherDecodeStrict'

toUserCookies :: [Cookie T.Text] -> Either String (M.Map T.Text T.Text)
toUserCookies xs = do
  let m =
        M.fromList
          . fmap (\Cookie {name, value} -> (name, value))
          $ filter isEssentialExercismCookie xs
  forM_ requiredKeys $ \k ->
    when (isNothing (m M.!? k)) $
      Left $ "Required key missing: " <> T.unpack k
  pure m
  where
    requiredKeys = ["_exercism_session", "user.id"]
    isEssentialExercismCookie Cookie {name, domain, path} =
      name `elem` requiredKeys
        && "exercism.io" `T.isSuffixOf` domain
        && path == "/"
