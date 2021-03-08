{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ExercismWizard.Execute.Overview
  ( getOverview
  , languagePairs
  , allExercises
  )
where

import Control.Arrow
import Control.Concurrent.Async
import Control.Monad hiding (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import qualified Data.Hashable
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock
import qualified ExercismWizard.Config as EWConf
import qualified ExercismWizard.Execute.Overview.RawExercise as RE
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.FilePath.Posix
import System.Random
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

languagePairs :: ArrowXml cat => cat XmlTree (String, String)
languagePairs =
  deep (divClassIs "joined-tracks")
    /> hasName "div"
    >>> hasAttrValue "class" (("tracks" `elem`) . words)
    >>> deep (hasName "a" >>> hasAttrValue "class" (== "track joined"))
    >>> (langName &&& langPath)
  where
    langName =
      getChildren
        >>> (hasName "div" >>> hasAttrValue "class" (== "title"))
        /> hasName "h2"
        /> getText
    langPath = getAttrValue "href"

processMyTracks :: Manager -> M.Map T.Text T.Text -> IO [(String, String)]
processMyTracks mgr userCookies =
  processWebPage mgr userCookies "/my/tracks" languagePairs

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

consumePrefix :: String -> String -> Maybe String
consumePrefix prefix input = ys <$ guard (prefix == xs)
  where
    (xs, ys) = splitAt (length prefix) input

mayProduce :: ArrowList cat => (a -> Maybe c) -> cat a c
mayProduce m = arrL (maybeToList . m)

coreExercises, sideExercises, allExercises :: ArrowXml cat => cat XmlTree RE.RawExercise
coreExercises =
  deep (divClassIs "core-exercises")
    /> divTag >>> mkExercise
  where
    mkExercise = proc node -> do
      status <- getStatus -< node
      name <- getTitle -< node
      (eId, uri) <- idAndHref -< node
      returnA -< RE.RawExercise {RE.status, RE.name, RE.eId, RE.uri, RE.core = True}
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
      eId <- getId -< node
      name <- getTitle -< node
      status <- getStatus -< node
      uri <- mayGetUri -< node
      returnA -< RE.RawExercise {RE.status, RE.name, RE.eId, RE.uri, RE.core = False}
    getStatus =
      getAttrValue "class" >>> mayProduce (consumePrefix "widget-side-exercise ")
    getId =
      getChildren
        >>> getAttrValue "id"
        >>> mayProduce (consumePrefix "exercise-")
allExercises = coreExercises <+> sideExercises

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
  -> IO [RE.RawExercise]
processMyTracksLang mgr userCookies (_langName, langPath) =
  processWebPage mgr userCookies langPath allExercises


-- TODO: use prettyprinter.
getOverview :: IO ()
getOverview = do
  EWConf.Config {EWConf.userCookies} <- EWConf.readConfig
  mgr <- newManager tlsManagerSettings
  ls <- processMyTracks mgr userCookies
  tasks <- mapM (\p -> async ((p,) <$> processMyTracksLang mgr userCookies p)) ls
  results <- mapM wait tasks
  forM_ results $ \((lName, _), es) -> do
    putStrLn $ "Overview on " <> lName <> " track: "
    let groupped = M.fromListWith (<>) $ fmap (\e@RE.RawExercise {RE.status} -> (status, [e])) es
    forM_ (M.toList groupped) $ \(k, vs) -> do
      putStrLn $ "  " <> k <> "(" <> show (length vs) <> "):"
      putStrLn $
        "    "
          <> case splitAt 5 (fmap RE.eId vs) of
            (xs, []) ->
              intercalate ", " xs
            (xs, ys) ->
              intercalate ", " xs <> ", and " <> show (length ys) <> " more"

-- getOverview = _extractTestDataTo "testdata/overview-extract"

redactLinks :: ArrowXml a => Int -> a XmlTree XmlTree
redactLinks salt =
  processTopDown $
    processAttrl
      (changeAttrValue redact `when` hasName "href")
      `when` (isElem >>> hasName "a")
      >>> processAttrl
        (changeAttrValue redact `when` (hasName "src" <+> hasName "onerror"))
        `when` (isElem >>> hasName "img")
      >>> processAttrl
        (changeAttrValue redact `when` hasName "style")
        `when` (isElem >>> divClassIs "img")
  where
    redact :: String -> String
    redact xs
      | "/my/solutions/" `isPrefixOf` xs =
        "/redacted/sol/" <> replaced
      | "https://assets" `isPrefixOf` xs =
        "/redacted/assets/" <> replaced
      | "background-image:" `isPrefixOf` xs = "-redacted-style: " <> replaced
      | "this.onerror=" `isPrefixOf` xs = "redacted=" <> replaced
      | otherwise = xs
      where
        replaced = show (Data.Hashable.hashWithSalt salt xs)

{-
  This function extract parts of interest of Exercism webpage and write them to files.
  Used for generating testdata.
 -}
_extractTestDataTo :: FilePath -> IO ()
_extractTestDataTo fpBase = do
  salt <- randomIO
  EWConf.Config {EWConf.userCookies} <- EWConf.readConfig
  mgr <- newManager tlsManagerSettings
  let writeDoc = writeDocument [withIndent yes]
      processBody p =
        processChildren
          (deep (hasName "body") >>> processChildren (redactLinks salt >>> p))

  lPairs <- do
    let xmlProc =
          processBody (deep (divClassIs "joined-tracks"))
            >>> writeDoc (fpBase </> "my-tracks.raw")
    processWebPage mgr userCookies "/my/tracks" (xmlProc >>> languagePairs)
  forM_ lPairs $ \(_, path) -> do
    let tr '/' = '-'
        tr c = c
        fp = fpBase </> fmap tr (tail path) <.> "raw"
        xmlProc =
          processBody
            (deep (divClassIs "core-exercises" <+> divClassIs "side-exercises"))
            >>> writeDoc fp
    processWebPage mgr userCookies path xmlProc

-- getOverview = _generateExpectedJsonFiles "testdata/overview-extract"

_generateExpectedJsonFiles :: FilePath -> IO ()
_generateExpectedJsonFiles fpBase = do
  let processAndWrite srcName arrow dstName = do
        xs <-
          runX $
            readDocument
              [withParseHTML yes, withWarnings no]
              (fpBase </> srcName)
              >>> arrow
        Aeson.encodeFile (fpBase </> dstName) xs

  processAndWrite "my-tracks.raw" languagePairs "my-tracks.expected.json"
  forM_ (words "go kotlin rust haskell scheme") $ \lang -> do
    let srcName = "my-tracks-" <> lang <> ".raw"
        dstName = "my-tracks-" <> lang <> ".expected.json"
    processAndWrite srcName allExercises dstName
