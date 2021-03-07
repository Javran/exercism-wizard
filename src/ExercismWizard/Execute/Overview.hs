{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ExercismWizard.Execute.Overview
  ( getOverview
  )
where

import Control.Arrow
import Control.Concurrent.Async
import Control.Monad
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
  -> IOStateArrow () XmlTree c
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
      xmlProc :: ArrowXml cat => cat XmlTree (String, String)
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
  , reIsCore :: Bool
  }
  deriving (Show)

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

consumePrefix :: String -> String -> Maybe String
consumePrefix prefix input = ys <$ guard (prefix == xs)
  where
    (xs, ys) = splitAt (length prefix) input

mayProduce :: ArrowList cat => (a -> Maybe c) -> cat a c
mayProduce m = arrL (maybeToList . m)

coreExercises, sideExercises :: ArrowXml cat => cat XmlTree RawExercise
coreExercises =
  deep (divClassIs "core-exercises")
    /> divTag >>> mkExercise
  where
    mkExercise = proc node -> do
      reStatus <- getStatus -< node
      reName <- getTitle -< node
      (reId, reUri) <- idAndHref -< node
      returnA -< RawExercise {reStatus, reName, reId, reUri, reIsCore = True}
    getStatus =
      getAttrValue "class" >>> mayProduce (consumePrefix "exercise-wrapper ")
    idAndHref =
      getChildren
        >>> hasAttrValue "class" (== "exercise")
        >>> (getId &&& mayGetUri)
      where
        getId = getAttrValue "id" >>> mayProduce (consumePrefix "exercise-")
sideExercises =
  deep (divClassIs "side-exercises")
    /> deep ((hasName "a" <+> divTag) >>> mkExercise)
  where
    mkExercise = proc node -> do
      reId <- getId -< node
      reName <- getTitle -< node
      reStatus <- getStatus -< node
      reUri <- mayGetUri -< node
      returnA -< RawExercise {reStatus, reName, reId, reUri, reIsCore = False}
    getStatus =
      getAttrValue "class" >>> mayProduce (consumePrefix "widget-side-exercise ")
    getId =
      getChildren
        >>> getAttrValue "id"
        >>> mayProduce (consumePrefix "exercise-")

divTag :: ArrowXml a => a XmlTree XmlTree
divTag = hasName "div"

divClassIs :: ArrowXml cat => String -> cat XmlTree XmlTree
divClassIs xs = divTag >>> hasAttrValue "class" (== xs)

mayGetUri :: ArrowXml a => a XmlTree (Maybe String)
mayGetUri = (hasName "a" >>> getAttrValue "href" >>> arr Just) <+> (divTag >>> constA Nothing)

getTitle :: ArrowXml a => a XmlTree String
getTitle = deep (divClassIs "title" /> getText >>> arr trim >>> isA (not . null))

processMyTracksLang
  :: Manager
  -> M.Map T.Text T.Text
  -> (String, String)
  -> IO [RawExercise]
processMyTracksLang mgr userCookies (_langName, langPath) =
  processWebPage mgr userCookies langPath (coreExercises <+> sideExercises)

getOverview :: IO ()
getOverview = do
  EWConf.Config {EWConf.userCookies} <- EWConf.readConfig
  mgr <- newManager tlsManagerSettings
  ls <- processMyTracks mgr userCookies
  tasks <- mapM (\p -> async ((p,) <$> processMyTracksLang mgr userCookies p)) ls
  results <- mapM wait tasks
  forM_ results $ \((lName, _), es) -> do
    putStrLn $ "Overview on " <> lName <> " track: "
    let groupped = M.fromListWith (<>) $ fmap (\e@RawExercise {reStatus} -> (reStatus, [e])) es
    forM_ (M.toList groupped) $ \(k, vs) -> do
      putStrLn $ "  " <> k <> ":"
      putStrLn $
        "    "
          <> case splitAt 5 (fmap reId vs) of
            (xs, []) ->
              intercalate ", " xs
            (xs, ys) ->
              intercalate ", " xs <> ", and " <> show (length ys) <> " more"
