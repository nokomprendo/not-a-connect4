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


