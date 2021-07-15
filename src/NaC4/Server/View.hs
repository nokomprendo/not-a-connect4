{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module NaC4.Server.View where

import qualified NaC4.Server.Params as Params

import qualified Data.Aeson as A
import qualified Data.Text as T
import GHC.Generics
import Lucid
import Text.RawString.QQ

-------------------------------------------------------------------------------
-- HomeData
-------------------------------------------------------------------------------

data HomeData = HomeData

instance ToHtml HomeData where
    toHtmlRaw = toHtml

    toHtml HomeData = doctypehtml_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            meta_ [ name_ "viewport"
                  , content_ "width=device-width,initial-scale=1,shrink-to-fit=no"]
            title_ "Not a Connect4"
            style_ "table, tr, th, td {border: 1px solid black; border-collapse: collapse}"
            style_ "th, td {padding: 0 10px 0 10px}"
            style_ "body {background-color: darkseagreen}"
            script_ [src_ "https://cdn.jsdelivr.net/npm/vega@5"] ("" :: T.Text)
            script_ [src_ "https://cdn.jsdelivr.net/npm/vega-lite@5"] ("" :: T.Text)
            script_ [src_ "https://cdn.jsdelivr.net/npm/vega-embed@5"] ("" :: T.Text)

        body_ $ do
            h1_ "Not a Connect4"

            h2_ "Summary"
            p_ $ do
                mkPlot "Games" descGames
                mkPlot "Results" descResults

            mkPlot "Users" descUsers
            mkPlot "Time" descTime

            h2_ "Users"
            table_ [id_ "idUsers"] mempty

            h2_ $ toHtml $ T.pack (show Params.viewNbResults) <> " last results"
            table_ [id_ "idResults"] mempty

            h2_ "Links"
            ul_ $ do
                li_ $ a_ [href_ "https://github.com/nokomprendo/not-a-connect4"] "source code"
                li_ $ a_ [href_ "api/results"] "api/results"
                li_ $ a_ [href_ "api/games-vg"] "api/games-vg"
                li_ $ a_ [href_ "api/users-vg"] "api/users-vg"

            script_ updateScript

mkPlot :: Monad m => T.Text -> T.Text -> HtmlT m ()
mkPlot name desc = 
    div_ [id_ ("plot" <> name)] 
        $ script_ $ "vegaEmbed('#plot" <> name <> "', " <> desc 
            <> ").then(p => window.view" <> name <> " = p.view);"

updateScript :: T.Text
updateScript = 
    [r|

    function fetch_json(url, jsonFunc) {
        fetch(url)
            .then(r => r.json())
            .then(jsonFunc);
    }

    function update_vg(jsonData, view) {
        let changeset = vega.changeset().remove(() => true).insert(jsonData);
        view.change('source_0', changeset).run();
    }

    function add_ths(node, labels) {
        row = document.createElement("tr");
        labels.map(l => 
            row.appendChild(document.createElement("th")).textContent = l);
        node.appendChild(row);
    }

    function add_tds(node, jsonData, labels) {
        row = document.createElement("tr");
        labels.map(l => 
            row.appendChild(document.createElement("td")).textContent = jsonData[l]);
        node.appendChild(row);
    }

    function my_update() {
        fetch_json("api/games-vg", jsonData => update_vg(jsonData, viewGames));
    
        fetch_json("api/users-vg", jsonData => {
            update_vg(jsonData, viewTime);
            update_vg(jsonData, viewUsers);

            idUsers.textContent = "";
            const fields = ["uUser","uWins","uLoses","uTies","uGames","uTime","uAvgTime"];
            add_ths(idUsers, fields);
            jsonData.map(user => add_tds(idUsers, user, fields));
        })
 
        fetch_json("api/results", jsonData => {
            update_vg(jsonData, viewResults);

            idResults.textContent = "";
            const fields = ["_rUserR","_rUserY","_rStatus","_rBoard","_rTimeR","_rTimeY"];
            add_ths(idResults, fields);
            const nbRes = |] <> T.pack (show Params.viewNbResults) <> [r|
            jsonData.slice(0,nbRes).map(result => add_tds(idResults, result, fields));
        })

    }

    const my_interval = setInterval(my_update, 2000);
    |]

-------------------------------------------------------------------------------
-- users
-------------------------------------------------------------------------------

data UsersVg = UsersVg
    { uUser :: T.Text
    , uWins :: Int
    , uLoses :: Int
    , uTies :: Int
    , uGames :: Int
    , uTime :: Double
    , uAvgTime :: Double
    } deriving (Generic)

instance A.ToJSON UsersVg

descUsers :: T.Text
descUsers = 
    [r| {
      "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
      "width": 600,
      "config": { "background": null },
      "data": {"url": "api/users-vg"},
      "transform": [
          {"fold": ["uWins", "uLoses", "uTies"], "as": ["result", "value"]},
          {"calculate": "if(datum.result === 'uWins', 0, if(datum.result === 'uLoses', 1, 2))", "as": "stats"}
      ],
      "mark": "bar",
      "encoding": {
          "y": {"type": "ordinal", "field": "uUser"},
          "x": {"type": "quantitative", "field": "value"},
          "color": {"type": "nominal", "field": "result",
              "scale": { "domain": [ "uWins", "uLoses", "uTies" ] }
          },
          "order": {"field": "stats"}
      }
    } |]

descTime :: T.Text
descTime = 
    [r| {
      "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
      "width": 600,
      "config": { "background": null },
      "data": {"url": "api/users-vg"},
      "mark": "bar",
      "encoding": {
        "y": {"type": "ordinal", "field": "uUser"},
        "x": {"type": "quantitative", "field": "uAvgTime"}
      }
    } |]

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
    [r| {
      "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
      "data": {"url": "api/games-vg"},
      "config": { 
        "view": {"step": 30},
        "axis": {"grid": true, "tickBand": "extent"},
        "background": null
      },
      "params": [{"name": "highlight", "select": "point"}],
      "mark": {"type": "rect"},
      "encoding": {
        "y": { "field": "gUserR", "type": "nominal" },
        "x": { "field": "gUserY", "type": "nominal" },
        "fill": {
          "scale": {"domainMin": 0, "scheme": "plasma"},
          "field": "gNbGames",
          "type": "quantitative"
        },
        "order": {"condition": {"param": "highlight", "value": 1}, "value": 0}
      }
    } |]

-------------------------------------------------------------------------------
-- results
-------------------------------------------------------------------------------

descResults :: T.Text
descResults = 
    [r| {
      "$schema": "https://vega.github.io/schema/vega-lite/v5.json",
      "data": {"url": "api/results"},
      "params": [{"name": "highlight", "select": "point"}],
      "mark": {"type": "rect"},
      "encoding": {
        "y": { "field": "_rUserR", "type": "nominal" },
        "x": { "field": "_rUserY", "type": "nominal" },
        "color": {
          "aggregate": "mean", 
          "field": "score",
          "scale": {"scheme": "plasma"}
        }
      },
      "transform": [
        {"calculate": "if(datum._rStatus === 'WinR', 1, if(datum._rStatus === 'Tie', 0, -1))", "as": "score"}
      ],
      "config": {
        "background": null,
        "view": {"step": 30},
        "axis": {"grid": true, "tickBand": "extent"}
      }
    } |]

