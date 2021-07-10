{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module NaC4.Server.HttpApp (httpApp) where

import NaC4.Protocol
import NaC4.Server.Model

import Control.Concurrent.STM
-- import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Lens.Micro.Platform
import Lucid
import Servant
import Servant.HTML.Lucid

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
    toHtml (HomeData (_res, _us)) = do
        h1_ "Not a Connect4"
        p_ $ a_ [href_ "https://github.com/nokomprendo/not-a-connect4"] "source code"
        p_ $ a_ [href_ "api/results"] "api/results"
        p_ $ a_ [href_ "api/users"] "api/users"
        -- TODO users
        -- TODO results
        -- ul_ $ forM_ cs $ \c -> li_ $ toHtml c

