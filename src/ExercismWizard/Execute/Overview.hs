{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ExercismWizard.Execute.Overview
  ( getOverview
  )
where

import Control.Arrow
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock
import qualified ExercismWizard.Config as EWConf
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Text.XML.HXT.Core
import Web.Cookie

getOverview :: IO ()
getOverview = do
  EWConf.Config {EWConf.userCookies} <- EWConf.readConfig
  now <- getCurrentTime
  mgr <- newManager tlsManagerSettings
  initReq <- parseRequest "https://exercism.io/my/tracks"
  let cj :: CookieJar
      cj = createCookieJar cs
      cs :: [Cookie]
      cs =
        fromJust
          . mapM ((\sc -> generateCookie sc initReq now False) . mkSetCookie)
          $ M.toList userCookies
      mkSetCookie (k, v) =
        defaultSetCookie
          { setCookieName = encodeUtf8 k
          , setCookieValue = encodeUtf8 v
          }
      req = initReq {cookieJar = Just cj}
  -- TODO: impl
  resp <- httpLbs req mgr
  let raw = T.unpack $ decodeUtf8 $ BSL.toStrict $ responseBody resp
  result <-
    runX $
      readString [withParseHTML yes, withWarnings no] raw
        >>> deep (hasName "div" >>> hasAttrValue "class" (== "joined-tracks"))
        /> hasName "div"
        >>> hasAttrValue "class" (("tracks" `elem`) . words)
        >>> deep (hasName "a" >>> hasAttrValue "class" (== "track joined"))
        >>> getChildren
        >>> (hasName "div" >>> hasAttrValue "class" (== "title"))
        >>> getChildren
        >>> getChildren
        >>> getText

  mapM_ print result
