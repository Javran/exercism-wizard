{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ExercismWizard.Execute.Overview
  ( getOverview
  )
where

import Control.Arrow
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.List
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

urlBase :: String
urlBase = "https://exercism.io"

processWebPage
  :: Manager
  -> M.Map T.Text T.Text
  -> String
  -> IOSLA (XIOState ()) XmlTree c
  -> IO [c]
processWebPage mgr userCookies rscPath xmlProc = do
  now <- getCurrentTime
  initReq <- parseRequest (urlBase <> rscPath)
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
  resp <- httpLbs req mgr
  let raw = T.unpack $ decodeUtf8 $ BSL.toStrict $ responseBody resp
  runX $
    readString [withParseHTML yes, withWarnings no] raw
      >>> xmlProc

processMyTracks :: Manager -> M.Map T.Text T.Text -> IO [(String, String)]
processMyTracks mgr userCookies = do
  let langName =
        getChildren
          >>> (hasName "div" >>> hasAttrValue "class" (== "title"))
          /> hasName "h2"
          /> getText
      langPath = getAttrValue "href"
      xmlProc =
        deep (hasName "div" >>> hasAttrValue "class" (== "joined-tracks"))
          /> hasName "div"
          >>> hasAttrValue "class" (("tracks" `elem`) . words)
          >>> deep (hasName "a" >>> hasAttrValue "class" (== "track joined"))
          >>> (langName &&& langPath)
  processWebPage mgr userCookies "/my/tracks" xmlProc

data RawExercise = RawExercise
  { reStatus :: String
  , reUri :: Maybe String
  , reName :: String
  , reId :: String
  }
  deriving (Show)

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

processMyTracksLang mgr userCookies (langName, langPath) = do
  putStrLn $ "Overview on " <> langName <> " track:"
  let coreExercises =
        deep (hasName "div" >>> hasAttrValue "class" (== "core-exercises"))
          /> hasName "div"
          >>> hasAttrValue "class" ("exercise-wrapper " `isPrefixOf`)
          >>> mkExercise
        where
          mkExercise = proc node -> do
            reStatus <- getStatus -< node
            reName <- getTitle -< node
            (reId, reUri) <- idAndHref -< node
            returnA -< RawExercise {reStatus, reName, reId, reUri}
          getStatus =
            getAttrValue "class" >>> arr (drop (length "exercise-wrapper "))
          getTitle =
            deep
              (hasName "div" >>> hasAttrValue "class" (== "title")
                 /> getText >>> arr trim >>> isA (not . null))
          idAndHref =
            getChildren
              >>> ((hasName "a" >>> clsExercise >>> (getId &&& (getAttrValue "href" >>> arr Just)))
                     <+> (hasName "div" >>> clsExercise >>> (getId &&& constA Nothing)))
            where
              clsExercise = hasAttrValue "class" (== "exercise")
              getId = getAttrValue "id" >>> arr (drop (length "exercise-"))

  rs <- processWebPage mgr userCookies langPath coreExercises
  mapM_ print rs

getOverview :: IO ()
getOverview = do
  EWConf.Config {EWConf.userCookies} <- EWConf.readConfig
  mgr <- newManager tlsManagerSettings
  result <- processMyTracks mgr userCookies
  mapM_ (processMyTracksLang mgr userCookies) result
