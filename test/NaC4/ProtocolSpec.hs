{-# LANGUAGE OverloadedStrings #-}

module NaC4.ProtocolSpec (main, spec) where

import NaC4.Game as G
import NaC4.Protocol as P

import Control.Monad.ST
import Data.Maybe
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "parseMsgToServer" $ do

        it "connect 1" $ do
            parseMsgToServer "connect foo bar \n" `shouldBe` Nothing
            parseMsgToServer "connect foo \n" `shouldBe` Just (Connect "foo")
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
            parseMsgToClient "genmove board R Tie \n" `shouldBe` Just (GenMove "board" PlayerR Tie)
            parseMsgToClient "genmove board Y WinR \n" `shouldBe` Just (GenMove "board" PlayerY WinR)
            parseMsgToClient "genmove board Z Tie \n" `shouldBe` Nothing
            parseMsgToClient "genmove board Y foo \n" `shouldBe` Nothing

        it "endgame 1" $ do
            parseMsgToClient "endgame board R WinR \n" `shouldBe` Just (EndGame "board" PlayerR WinR)
            parseMsgToClient "endgame board R WinY \n" `shouldBe` Just (EndGame "board" PlayerR WinY)
            parseMsgToClient "endgame board Y PlayR \n" `shouldBe` Just (EndGame "board" PlayerY PlayR)
            parseMsgToClient "endgame board Y PlayY \n" `shouldBe` Just (EndGame "board" PlayerY PlayY)
            parseMsgToClient "endgame board Y Tie \n" `shouldBe` Just (EndGame "board" PlayerY Tie)
            parseMsgToClient "endgame board Y foo \n" `shouldBe` Nothing

    describe "fmtMsgToServer" $ do

        it "connect 1" $ do
            fmtMsgToServer (Connect "foo")`shouldBe` "connect foo \n"

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
            fmtMsgToClient (GenMove "board" PlayerR Tie)`shouldBe` "genmove board R Tie \n"
            fmtMsgToClient (GenMove "board" PlayerY WinR)`shouldBe` "genmove board Y WinR \n"

        it "endgame 1" $ do
            fmtMsgToClient (EndGame "board" PlayerR WinR)`shouldBe` "endgame board R WinR \n"
            fmtMsgToClient (EndGame "board" PlayerR WinY)`shouldBe` "endgame board R WinY \n"
            fmtMsgToClient (EndGame "board" PlayerR PlayR)`shouldBe` "endgame board R PlayR \n"
            fmtMsgToClient (EndGame "board" PlayerY PlayY)`shouldBe` "endgame board Y PlayY \n"
            fmtMsgToClient (EndGame "board" PlayerY Tie)`shouldBe` "endgame board Y Tie \n"

    describe "fromGame" $ do

        it "1" $ do
            (b, p, s) <- stToIO (mkGame PlayerR 
                        >>= playK 2 >>= playK 4
                        >>= playK 2 >>= playK 4
                        >>= playK 2 
                        >>= fromGame)
            p `shouldBe` PlayerY
            s `shouldBe` PlayY
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
                p = PlayerY
                s = PlayY
            Just g <- stToIO (toGame b p s)
            _currentPlayer g `shouldBe` PlayerY

        it "2" $ do
            let b1 = "..R.Y..\
                     \..R.Y..\
                     \..R....\
                     \.......\
                     \.......\
                     \......."
                p1 = PlayerY
                s1 = PlayY
            (b2, p2, s2) <- stToIO (toGame b1 p1 s1 >>= fromGame . fromJust)
            b1 `shouldBe` b2
            p1 `shouldBe` p2
            s1 `shouldBe` s2

