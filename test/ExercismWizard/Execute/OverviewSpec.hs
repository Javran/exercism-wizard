module ExercismWizard.Execute.OverviewSpec where

import Control.Monad
import Data.Aeson
import ExercismWizard.Execute.Overview
import System.FilePath.Posix
import Test.Hspec
import Text.XML.HXT.Core

fpBase :: FilePath
fpBase = "testdata/overview-extract/"

spec :: Spec
spec = do
  let verifyExample arrow basename = do
        results <-
          runX $
            readDocument
              [withParseHTML yes, withWarnings no]
              (fpBase </> basename <.> "raw")
              >>> arrow
        Just expected <- decodeFileStrict' (fpBase </> basename <.> "expected.json")
        results `shouldBe` expected
  describe "Extract from /my/tracks" $ do
    specify "languagePairs" $
      verifyExample languagePairs "my-tracks"
  describe "Extract from /my/tracks/<language>" $
    forM_ (words "go haskell kotlin rust scheme") $ \lang ->
      specify lang $
        verifyExample allExercises ("my-tracks-" <> lang)
