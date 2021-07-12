{-# LANGUAGE OverloadedStrings #-}

module NaC4.Server.ModelSpec (main, spec) where

import Control.Monad.ST
import Test.Hspec

import NaC4.Server.Model
import NaC4.Game

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "userInBattle" $ do

        it "1" $ do
            g <- stToIO $ mkGame PlayerR
            userInBattle "foo" (Battle "foo" "bar" g 13 37 42) `shouldBe` True

        it "2" $ do
            g <- stToIO $ mkGame PlayerR
            userInBattle "bar" (Battle "foo" "bar" g 13 37 42) `shouldBe` True

        it "3" $ do
            g <- stToIO $ mkGame PlayerR
            userInBattle "baz" (Battle "foo" "bar" g 13 37 42) `shouldBe` False

    describe "opponent" $ do

        it "1" $ do
            g <- stToIO $ mkGame PlayerR
            opponent "foo" (Battle "foo" "bar" g 13 37 42) `shouldBe` "bar"

        it "2" $ do
            g <- stToIO $ mkGame PlayerR
            opponent "bar" (Battle "foo" "bar" g 13 37 42) `shouldBe` "foo"

    describe "Eq battle" $ do

        it "1" $ do
            g <- stToIO $ mkGame PlayerR
            (Battle "foo" "bar" g 13 37 42 == Battle "foo" "bar" g 13 37 42) `shouldBe` True

        it "2" $ do
            g <- stToIO $ mkGame PlayerR
            (Battle "foo" "bar" g 0 37 0 == Battle "foo" "bar" g 13 37 42) `shouldBe` True

        it "3" $ do
            g <- stToIO $ mkGame PlayerR
            (Battle "foo" "foo" g 13 0 0 == Battle "foo" "bar" g 13 37 42) `shouldBe` False

        it "4" $ do
            g <- stToIO $ mkGame PlayerR
            (Battle "bar" "bar" g 13 37 0 == Battle "foo" "bar" g 13 37 42) `shouldBe` False

    describe "updateStats" $ do

        it "1" $ do
            updateStats PlayerR WinR 1 (UserStats 1 2 3 6 1) `shouldBe` UserStats 2 2 3 7 2
            updateStats PlayerR WinY 0 (UserStats 1 2 3 6 1) `shouldBe` UserStats 1 3 3 7 1
            updateStats PlayerR Tie 2 (UserStats 1 2 3 6 0) `shouldBe` UserStats 1 2 4 7 2

        it "2" $ do
            updateStats PlayerY WinR 0 (UserStats 1 2 3 6 0) `shouldBe` UserStats 1 3 3 7 0
            updateStats PlayerY WinY 0 (UserStats 1 2 3 6 0) `shouldBe` UserStats 2 2 3 7 0
            updateStats PlayerY Tie 0 (UserStats 1 2 3 6 0) `shouldBe` UserStats 1 2 4 7 0

