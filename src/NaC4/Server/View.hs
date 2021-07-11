{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module NaC4.Server.View where

import NaC4.Protocol
import NaC4.Server.Model

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics
import Lens.Micro.Platform
import Lucid
import Text.RawString.QQ

-------------------------------------------------------------------------------
-- HomeData
-------------------------------------------------------------------------------

nbResults :: Int
nbResults = 100

newtype HomeData = HomeData ([Result], M.Map User UserStats)

instance ToHtml HomeData where
    toHtmlRaw = toHtml
    toHtml (HomeData (results, users)) = doctypehtml_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            meta_ [ name_ "viewport"
                  , content_ "width=device-width,initial-scale=1,shrink-to-fit=no"]
            title_ "Not a Connect4"
            style_ "table, tr, th, td {border: 1px solid black; border-collapse: collapse}"
            style_ "th, td {padding: 0 10px 0 10px}"
            script_ [src_ "https://cdn.jsdelivr.net/npm/vega@5"] ("" :: T.Text)
            script_ [src_ "https://cdn.jsdelivr.net/npm/vega-lite@5"] ("" :: T.Text)
            script_ [src_ "https://cdn.jsdelivr.net/npm/vega-embed@6"] ("" :: T.Text)
        body_ $ do
            h1_ "Not a Connect4"
            ul_ $ do
                li_ $ a_ [href_ "https://github.com/nokomprendo/not-a-connect4"] "source code"
                li_ $ a_ [href_ "api/users"] "api/users"
                li_ $ a_ [href_ "api/results"] "api/results"
                li_ $ a_ [href_ "api/games-vg"] "api/games-vg"
                li_ $ a_ [href_ "api/users-vg"] "api/users-vg"
                li_ $ a_ [href_ "api/time-vg"] "api/time-vg"

            h2_ "Summary"

            p_ $ div_ [id_ "plotGames"] $ script_ $ 
                "vegaEmbed('#plotGames', " <> descGames <> ");"

            p_ $ div_ [id_ "plotUsers"] $ script_ $ 
                "vegaEmbed('#plotUsers', " <> descUsers <> ");"

            p_ $ div_ [id_ "plotTime"] $ script_ $ 
                "vegaEmbed('#plotTime', " <> descTime <> ");"

            h2_ "Users"
            table_ $ do
                tr_ $ mapM_ th_ [ "user", "wins", "loses", "ties", "games" ]
                forM_ (M.toAscList users) $ \(u, us) -> tr_ $ mapM_ (td_ . toHtml) 
                    (u : map (T.pack . show) 
                        [us^.usWins, us^.usLoses, us^.usTies, us^.usGames])

            h2_ $ toHtml $ "Results (" <> T.pack (show nbResults) <> " last ones)"
            table_ $ do
                tr_ $ mapM_ th_ [ "red", "yellow", "status", "board" ]
                forM_ (take nbResults results) $ \res -> tr_ $ mapM_ (td_ . toHtml) 
                    [ res^.rUserR, res^.rUserY, T.pack (show $ res^.rStatus), res^.rBoard ]

-------------------------------------------------------------------------------
-- users
-------------------------------------------------------------------------------

data UsersVg = UsersVg
    { uUser :: T.Text
    , uWins :: Int
    , uLoses :: Int
    , uTies :: Int
    } deriving (Generic)

instance A.ToJSON UsersVg

descUsers :: T.Text
descUsers = 
    [r|
        {
          "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
          "width": 600,
          "data": {"url": "api/users-vg"},
          "transform": [
            {"fold": ["uWins", "uLoses", "uTies"], "as": ["result", "value"]},
            {"calculate": "if(datum.result === 'uWins', 0, if(datum.result === 'uLoses', 1, 2))", "as": "siteOrder"}
            ],
          "mark": "bar",
          "encoding": {
            "y": {"type": "ordinal", "field": "uUser"},
            "x": {"type": "quantitative", "field": "value"},
            "color": {"type": "nominal", "field": "result",
                "scale": { "domain": [ "uWins", "uLoses", "uTies" ] }
            },
            "order": {"field": "siteOrder"}
          }
        }
    |]

-------------------------------------------------------------------------------
-- games
-------------------------------------------------------------------------------

data GamesVg = GamesVg
    { gUserR :: T.Text
    , gUserY :: T.Text
    , gNbGames :: Int
    } deriving (Generic)

instance A.ToJSON GamesVg

descGames :: T.Text
descGames = 
    [r|
        {
          "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
          "data": {"url": "api/games-vg"},
          "params": [{"name": "highlight", "select": "point"}],
          "mark": {"type": "rect"},
          "encoding": {
            "y": { "field": "gUserR", "type": "nominal" },
            "x": { "field": "gUserY", "type": "nominal" },
            "fill": { "field": "gNbGames", "type": "quantitative" },
            "order": {"condition": {"param": "highlight", "value": 1}, "value": 0}
          },
          "config": { "view": {"step": 40} }
        }
    |]

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

data TimeVg = TimeVg
    { tUser :: T.Text
    , tTime :: Double
    , tGames :: Int
    , tAvg :: Double
    } deriving (Generic)

instance A.ToJSON TimeVg

descTime :: T.Text
descTime = 
    [r|
        {
          "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
          "width": 600,
          "data": {"url": "api/time-vg"},
          "mark": "bar",
          "encoding": {
            "y": {"type": "ordinal", "field": "tUser"},
            "x": {"type": "quantitative", "field": "tAvg"}
          }
        }
    |]

