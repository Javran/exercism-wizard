{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module ExercismWizard.Execute.Overview
  ( getOverview
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import qualified ExercismWizard.Config as EWConf
import Network.HTTP.Client
import Network.HTTP.Client.TLS
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
  print (responseBody resp)
