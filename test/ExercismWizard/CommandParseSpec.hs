{-# LANGUAGE OverloadedStrings #-}

module ExercismWizard.CommandParseSpec where

import qualified Data.Text as T
import ExercismWizard.CommandParse
import ExercismWizard.Types
import Options.Applicative
import Test.Hspec

spec :: Spec
spec = describe "parseArgs" $ do
  let parseArgsW = parseArgs' . words
      parseArgs' :: [String] -> Either [T.Text] (Maybe Command)
      parseArgs' = fmap getParseResult . parseArgs
      rj = Right . Just
  specify "CmdProxy" $ do
    parseArgsW "proxy foo"
      `shouldBe` rj (CmdProxy ["foo"])
    parseArgsW "proxy --foo -- -z"
      `shouldBe` rj (CmdProxy ["--foo", "--", "-z"])
  specify "CmdLangAction" $ do
    parseArgsW "test -- zzz -a --b"
      `shouldBe` rj (
                    CmdLangAction
                      Test
                      (RawExercise (Nothing, Nothing))
                      ["zzz", "-a", "--b"])
    parseArgsW "lint -- t"
      `shouldBe` rj (
                    CmdLangAction
                      Lint
                      (RawExercise (Nothing, Nothing))
                      ["t"])
    parseArgsW "fmt"
      `shouldBe` rj (
                    CmdLangAction
                      Format
                      (RawExercise (Nothing, Nothing))
                      [])
  specify "CmdLangAction, no extra before --" $ do
    parseArgsW "fmt x y z" `shouldBe` Right Nothing
    parseArgsW "lint z 123 --" `shouldBe` Right Nothing
  specify "Command but cannot attach extra args" $ do
    parseArgsW "on -- a" `shouldBe` Left ["a"]
    parseArgsW "get --" `shouldBe` Left []
  specify "<lang>:<track>" $ do
    -- test with "on" command
    let testcase input pair =
          parseArgsW input
            `shouldBe` rj (CmdOn (RawExercise pair))
    testcase "on" (Nothing, Nothing)
    testcase "on hs:" (Just Haskell, Nothing)
    testcase "on :wow" (Nothing, Just "wow")
    testcase "on :" (Nothing, Nothing)
    testcase "on rs:zzz" (Just Rust, Just "zzz")
  specify "@debug" $
    parseArgsW "@debug 1 -- x y" `shouldBe` Right (Just $ CmdDebug ["1", "--", "x", "y"])
