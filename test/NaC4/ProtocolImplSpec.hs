{-# LANGUAGE OverloadedStrings #-}

module NaC4.ProtocolImplSpec (main, spec) where

import NaC4.Game as G
import NaC4.Protocol as P
import NaC4.ProtocolImpl

import Control.Monad.ST
import Data.Maybe
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "parseProtocol" $ do

        it "connect 1" $ do
            parseProtocol "connect foo bar \n" `shouldBe` Just (Connect "foo" "bar")
            parseProtocol "connect foo \n" `shouldBe` Nothing
            parseProtocol "connect foo bar baz \n" `shouldBe` Nothing

        it "connected 1" $ do
            parseProtocol "connected foo bar \n" `shouldBe` Just (Connected "foo bar")
            parseProtocol "connected\n" `shouldBe` Just (Connected "")

        it "not-connected 1" $ do
            parseProtocol "not-connected foo bar \n" `shouldBe` Just (NotConnected "foo bar")
            parseProtocol "not-connected\n" `shouldBe` Just (NotConnected "")

        it "newgame 1" $ do
            parseProtocol "newgame foo bar \n" `shouldBe` Just (NewGame "foo" "bar")
            parseProtocol "newgame foo \n" `shouldBe` Nothing
            parseProtocol "newgame foo bar baz \n" `shouldBe` Nothing

        it "genmove 1" $ do
            parseProtocol "genmove board R \n" `shouldBe` Just (GenMove "board" ColorR)
            parseProtocol "genmove board Y \n" `shouldBe` Just (GenMove "board" ColorY)
            parseProtocol "genmove board Z \n" `shouldBe` Nothing
            parseProtocol "genmove board Y foo \n" `shouldBe` Nothing

        it "playmove 1" $ do
            parseProtocol "playmove 2 \n" `shouldBe` Just (PlayMove 2)
            parseProtocol "playmove 42 \n" `shouldBe` Just (PlayMove 42)
            parseProtocol "playmove foo \n" `shouldBe` Nothing

        it "endgame 1" $ do
            parseProtocol "endgame board WinR \n" `shouldBe` Just (EndGame "board" P.WinR)
            parseProtocol "endgame board WinY \n" `shouldBe` Just (EndGame "board" P.WinY)
            parseProtocol "endgame board Draw \n" `shouldBe` Just (EndGame "board" P.Draw)
            parseProtocol "endgame board foo \n" `shouldBe` Nothing

    describe "fmtProtocol" $ do

        it "connect 1" $ do
            fmtProtocol (Connect "foo" "bar")`shouldBe` "connect foo bar \n"

        it "connected 1" $ do
            fmtProtocol (Connected "foo bar")`shouldBe` "connected foo bar \n"

        it "not-connected 1" $ do
            fmtProtocol (NotConnected "foo bar")`shouldBe` "not-connected foo bar \n"

        it "newgame 1" $ do
            fmtProtocol (NewGame "foo" "bar")`shouldBe` "newgame foo bar \n"

        it "genmove 1" $ do
            fmtProtocol (GenMove "board" ColorR)`shouldBe` "genmove board R \n"
            fmtProtocol (GenMove "board" ColorY)`shouldBe` "genmove board Y \n"

        it "playmove 1" $ do
            fmtProtocol (PlayMove 2)`shouldBe` "playmove 2 \n"
            fmtProtocol (PlayMove 42)`shouldBe` "playmove 42 \n"

        it "endgame 1" $ do
            fmtProtocol (EndGame "board" P.WinR)`shouldBe` "endgame board WinR \n"
            fmtProtocol (EndGame "board" P.WinY)`shouldBe` "endgame board WinY \n"
            fmtProtocol (EndGame "board" P.Draw)`shouldBe` "endgame board Draw \n"

    describe "fromGame" $ do

        it "1" $ do
            (b, c) <- stToIO (mkGame PlayerR 
                        >>= playK 2 >>= playK 4
                        >>= playK 2 >>= playK 4
                        >>= playK 2 
                        >>= fromGame)
            c `shouldBe` ColorY
            b `shouldBe`
                "..R.Y..\
                \..R.Y..\
                \..R....\
                \.......\
                \.......\
                \......."

    describe "toGame" $ do

        it "1" $ do
            let b = "..R.Y..\
                    \..R.Y..\
                    \..R....\
                    \.......\
                    \.......\
                    \......."
                c = ColorY
            Just g <- stToIO (toGame b c)
            _currentPlayer g `shouldBe` PlayerY

        it "2" $ do
            let b1 = "..R.Y..\
                     \..R.Y..\
                     \..R....\
                     \.......\
                     \.......\
                     \......."
                c1 = ColorY
            (b2, c2) <- stToIO (toGame b1 c1 >>= fromGame . fromJust)
            b1 `shouldBe` b2
            c1 `shouldBe` c2

