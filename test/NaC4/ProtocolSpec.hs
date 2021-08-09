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
            parseMsgToClient "genmove board R Tie 0.5 \n" `shouldBe` Just (GenMove "board" PlayerR Tie 0.5)
            parseMsgToClient "genmove board Y WinR 2 \n" `shouldBe` Just (GenMove "board" PlayerY WinR 2)
            parseMsgToClient "genmove board Z Tie \n" `shouldBe` Nothing
            parseMsgToClient "genmove board Y foo \n" `shouldBe` Nothing

        it "endgame 1" $ do
            parseMsgToClient "endgame board R WinR Ok \n" `shouldBe` Just (EndGame "board" PlayerR WinR Ok)
            parseMsgToClient "endgame board R WinY Ok \n" `shouldBe` Just (EndGame "board" PlayerR WinY Ok)
            parseMsgToClient "endgame board Y PlayR Ok \n" `shouldBe` Just (EndGame "board" PlayerY PlayR Ok)
            parseMsgToClient "endgame board Y PlayY Timeout \n" `shouldBe` Just (EndGame "board" PlayerY PlayY Timeout)
            parseMsgToClient "endgame board Y Tie Timeout \n" `shouldBe` Just (EndGame "board" PlayerY Tie Timeout)
            parseMsgToClient "endgame board Y foo Timeout \n" `shouldBe` Nothing
            parseMsgToClient "endgame board Y Tie foo \n" `shouldBe` Nothing 

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
            fmtMsgToClient (GenMove "board" PlayerR Tie 3)`shouldBe` "genmove board R Tie 3.0 \n"
            fmtMsgToClient (GenMove "board" PlayerY WinR 4)`shouldBe` "genmove board Y WinR 4.0 \n"
            fmtMsgToClient (GenMove "board" PlayerY WinR 4.2)`shouldBe` "genmove board Y WinR 4.2 \n"
            fmtMsgToClient (GenMove "board" PlayerY WinR 1.337)`shouldBe` "genmove board Y WinR 1.3 \n"

        it "endgame 1" $ do
            fmtMsgToClient (EndGame "board" PlayerR WinR Ok)`shouldBe` "endgame board R WinR Ok \n"
            fmtMsgToClient (EndGame "board" PlayerR WinY Ok)`shouldBe` "endgame board R WinY Ok \n"
            fmtMsgToClient (EndGame "board" PlayerR PlayR Ok)`shouldBe` "endgame board R PlayR Ok \n"
            fmtMsgToClient (EndGame "board" PlayerY PlayY Timeout)`shouldBe` "endgame board Y PlayY Timeout \n"
            fmtMsgToClient (EndGame "board" PlayerY Tie Timeout)`shouldBe` "endgame board Y Tie Timeout \n"

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

