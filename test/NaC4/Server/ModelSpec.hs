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

    describe "isInBattle" $ do

        it "1" $ do
            g <- stToIO $ mkGame PlayerR
            isInBattle "foo" (Battle "foo" "bar" g 42) `shouldBe` True

        it "2" $ do
            g <- stToIO $ mkGame PlayerR
            isInBattle "bar" (Battle "foo" "bar" g 42) `shouldBe` True

        it "3" $ do
            g <- stToIO $ mkGame PlayerR
            isInBattle "baz" (Battle "foo" "bar" g 42) `shouldBe` False

    describe "opponent" $ do

        it "1" $ do
            g <- stToIO $ mkGame PlayerR
            opponent "foo" (Battle "foo" "bar" g 42) `shouldBe` "bar"

        it "2" $ do
            g <- stToIO $ mkGame PlayerR
            opponent "bar" (Battle "foo" "bar" g 42) `shouldBe` "foo"

    describe "Eq battle" $ do

        it "1" $ do
            g <- stToIO $ mkGame PlayerR
            (Battle "foo" "bar" g 42 == Battle "foo" "bar" g 42) `shouldBe` True

        it "2" $ do
            g <- stToIO $ mkGame PlayerR
            (Battle "foo" "bar" g 0 == Battle "foo" "bar" g 42) `shouldBe` True

        it "3" $ do
            g <- stToIO $ mkGame PlayerR
            (Battle "foo" "foo" g 0 == Battle "foo" "bar" g 42) `shouldBe` False

        it "4" $ do
            g <- stToIO $ mkGame PlayerR
            (Battle "bar" "bar" g 0 == Battle "foo" "bar" g 42) `shouldBe` False

    describe "updateStats" $ do

        it "1" $ do
            updateStats PlayerR WinR (UserStats 1 2 3 6) `shouldBe` UserStats 2 2 3 7
            updateStats PlayerR WinY (UserStats 1 2 3 6) `shouldBe` UserStats 1 3 3 7
            updateStats PlayerR Tie (UserStats 1 2 3 6) `shouldBe` UserStats 1 2 4 7

        it "2" $ do
            updateStats PlayerY WinR (UserStats 1 2 3 6) `shouldBe` UserStats 1 3 3 7
            updateStats PlayerY WinY (UserStats 1 2 3 6) `shouldBe` UserStats 2 2 3 7
            updateStats PlayerY Tie (UserStats 1 2 3 6) `shouldBe` UserStats 1 2 4 7

