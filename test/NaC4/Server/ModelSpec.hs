{-# LANGUAGE OverloadedStrings #-}

module NaC4.Server.ModelSpec (main, spec) where

import Test.Hspec

import NaC4.Server.Model
import NaC4.Game

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "userInBattleKey" $ do

        it "1" $ do
            userInBattleKey "foo" ("foo", "bar") `shouldBe` True

        it "2" $ do
            userInBattleKey "bar" ("foo","bar") `shouldBe` True

        it "3" $ do
            userInBattleKey "baz" ("foo","bar") `shouldBe` False

    describe "opponent" $ do

        it "1" $ do
            opponent "foo" ("foo","bar") `shouldBe` "bar"

        it "2" $ do
            opponent "bar" ("foo","bar") `shouldBe` "foo"

    describe "updateStats" $ do

        it "1" $ do
            updateStats PlayerR WinR 1 (UserStats 1 2 3 6 1) `shouldBe` UserStats 2 2 3 7 2
            updateStats PlayerR WinY 0 (UserStats 1 2 3 6 1) `shouldBe` UserStats 1 3 3 7 1
            updateStats PlayerR Tie 2 (UserStats 1 2 3 6 0) `shouldBe` UserStats 1 2 4 7 2

        it "2" $ do
            updateStats PlayerY WinR 0 (UserStats 1 2 3 6 0) `shouldBe` UserStats 1 3 3 7 0
            updateStats PlayerY WinY 0 (UserStats 1 2 3 6 0) `shouldBe` UserStats 2 2 3 7 0
            updateStats PlayerY Tie 0 (UserStats 1 2 3 6 0) `shouldBe` UserStats 1 2 4 7 0

