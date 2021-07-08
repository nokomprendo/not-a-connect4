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

    describe "parseMsgToServer" $ do

        it "connect 1" $ do
            parseMsgToServer "connect foo bar \n" `shouldBe` Just (Connect "foo" "bar")
            parseMsgToServer "connect foo \n" `shouldBe` Nothing
            parseMsgToServer "connect foo bar baz \n" `shouldBe` Nothing

        it "playmove 1" $ do
            parseMsgToServer "playmove 2 \n" `shouldBe` Just (PlayMove 2)
            parseMsgToServer "playmove 42 \n" `shouldBe` Just (PlayMove 42)
            parseMsgToServer "playmove foo \n" `shouldBe` Nothing

    describe "parseMsgToClient" $ do

        it "connected 1" $ do
            parseMsgToClient "connected foo bar \n" `shouldBe` Just (Connected "foo bar")
            parseMsgToClient "connected\n" `shouldBe` Just (Connected "")

        it "not-connected 1" $ do
            parseMsgToClient "not-connected foo bar \n" `shouldBe` Just (NotConnected "foo bar")
            parseMsgToClient "not-connected\n" `shouldBe` Just (NotConnected "")

        it "newgame 1" $ do
            parseMsgToClient "newgame foo bar \n" `shouldBe` Just (NewGame "foo" "bar")
            parseMsgToClient "newgame foo \n" `shouldBe` Nothing
            parseMsgToClient "newgame foo bar baz \n" `shouldBe` Nothing

        it "genmove 1" $ do
            parseMsgToClient "genmove board R \n" `shouldBe` Just (GenMove "board" ColorR)
            parseMsgToClient "genmove board Y \n" `shouldBe` Just (GenMove "board" ColorY)
            parseMsgToClient "genmove board Z \n" `shouldBe` Nothing
            parseMsgToClient "genmove board Y foo \n" `shouldBe` Nothing

        it "endgame 1" $ do
            parseMsgToClient "endgame board WinR \n" `shouldBe` Just (EndGame "board" P.WinR)
            parseMsgToClient "endgame board WinY \n" `shouldBe` Just (EndGame "board" P.WinY)
            parseMsgToClient "endgame board Draw \n" `shouldBe` Just (EndGame "board" P.Draw)
            parseMsgToClient "endgame board foo \n" `shouldBe` Nothing

    describe "fmtMsgToServer" $ do

        it "connect 1" $ do
            fmtMsgToServer (Connect "foo" "bar")`shouldBe` "connect foo bar \n"

        it "playmove 1" $ do
            fmtMsgToServer (PlayMove 2)`shouldBe` "playmove 2 \n"
            fmtMsgToServer (PlayMove 42)`shouldBe` "playmove 42 \n"

    describe "fmtMsgToClient" $ do

        it "connected 1" $ do
            fmtMsgToClient (Connected "foo bar")`shouldBe` "connected foo bar \n"

        it "not-connected 1" $ do
            fmtMsgToClient (NotConnected "foo bar")`shouldBe` "not-connected foo bar \n"

        it "newgame 1" $ do
            fmtMsgToClient (NewGame "foo" "bar")`shouldBe` "newgame foo bar \n"

        it "genmove 1" $ do
            fmtMsgToClient (GenMove "board" ColorR)`shouldBe` "genmove board R \n"
            fmtMsgToClient (GenMove "board" ColorY)`shouldBe` "genmove board Y \n"

        it "endgame 1" $ do
            fmtMsgToClient (EndGame "board" P.WinR)`shouldBe` "endgame board WinR \n"
            fmtMsgToClient (EndGame "board" P.WinY)`shouldBe` "endgame board WinY \n"
            fmtMsgToClient (EndGame "board" P.Draw)`shouldBe` "endgame board Draw \n"

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

