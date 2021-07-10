{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module NaC4.Server.HttpApp (httpApp) where

import NaC4.Protocol
import NaC4.Server.Model

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Lens.Micro.Platform
import Lucid
import Servant
import Servant.HTML.Lucid

nbResults :: Int
nbResults = 100

newtype HomeData = HomeData ([Result], M.Map User UserStats)

type ApiResultsRoute = "api" :> "results" :> Get '[JSON] [Result]
type ApiUsersRoute = "api" :> "users" :> Get '[JSON] (M.Map User UserStats)
type HomeRoute = Get '[HTML] HomeData

type ServerApi
    =    ApiResultsRoute
    :<|> ApiUsersRoute
    :<|> HomeRoute

handleServerApi :: TVar Model -> Server ServerApi
handleServerApi modelVar
    =    handleResults modelVar
    :<|> handleUsers modelVar
    :<|> handleHome modelVar

handleResults :: TVar Model -> Handler [Result]
handleResults modelVar = liftIO (_mResults <$> readTVarIO modelVar)

handleUsers :: TVar Model -> Handler (M.Map User UserStats)
handleUsers modelVar = liftIO (_mUserStats <$> readTVarIO modelVar)

handleHome :: TVar Model -> Handler HomeData
handleHome modelVar = do
    m <- liftIO $ readTVarIO modelVar
    pure $ HomeData (m^.mResults, m^.mUserStats)

httpApp :: TVar Model -> Application
httpApp modelVar = serve (Proxy @ServerApi) (handleServerApi modelVar)

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
        body_ $ do
            h1_ "Not a Connect4"
            p_ $ do
                a_ [href_ "https://github.com/nokomprendo/not-a-connect4"] "source code"
                " - "
                a_ [href_ "api/users"] "api/users"
                " - "
                a_ [href_ "api/results"] "api/results"

            h2_ "Users"
            table_ $ do
                tr_ $ mapM_ th_ [ "user", "wins", "loses", "ties", "games" ]
                forM_ (M.toAscList users) $ \(u, us) -> tr_ $ mapM_ (td_ . toHtml) 
                    (u : map (T.pack . show) 
                        [us^.usWins, us^.usLoses, us^.usTies, us^.usGames])

            h2_ $ toHtml $ "Results (" <> T.pack (show nbResults) <> " last ones)"
            table_ $ do
                tr_ $ mapM_ th_ [ "red", "yellow", "status", "board" ]
                forM_ (take nbResults results) $ \r -> tr_ $ mapM_ (td_ . toHtml) 
                    [ r^.rUserR, r^.rUserY, T.pack (show $ r^.rStatus), r^.rBoard ]

