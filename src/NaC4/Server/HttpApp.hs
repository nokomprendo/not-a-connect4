{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module NaC4.Server.HttpApp (httpApp) where

import NaC4.Protocol
import NaC4.Server.Model
import NaC4.Server.View

import Control.Concurrent.STM
import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Lens.Micro.Platform
import Servant
import Servant.HTML.Lucid

type ApiResultsRoute = "api" :> "results" :> Get '[JSON] [Result]
type ApiUsersRoute = "api" :> "users" :> Get '[JSON] (M.Map User UserStats)
type ApiUsersVgRoute = "api" :> "users-vg" :> Get '[JSON] [UsersVg]
type ApiNbGamesRoute = "api" :> "nb-games" :> Get '[JSON] (M.Map (User, User) Int)
type HomeRoute = Get '[HTML] HomeData

type ServerApi
    =    ApiResultsRoute
    :<|> ApiUsersRoute
    :<|> ApiUsersVgRoute
    :<|> ApiNbGamesRoute
    :<|> HomeRoute

handleServerApi :: TVar Model -> Server ServerApi
handleServerApi modelVar
    =    handleGetInModel _mResults modelVar
    :<|> handleGetInModel _mUserStats modelVar
    :<|> handleUsersVg modelVar
    :<|> handleNbGamesVg modelVar
    :<|> handleHome modelVar

handleGetInModel :: (Model -> a) -> TVar Model -> Handler a
handleGetInModel f modelVar = liftIO (f <$> readTVarIO modelVar)

-- TODO
handleNbGamesVg :: TVar Model -> Handler (M.Map (User, User) Int)
handleNbGamesVg modelVar = liftIO (_mNbGames <$> readTVarIO modelVar)

handleUsersVg :: TVar Model -> Handler [UsersVg]
handleUsersVg modelVar = do
    stats <- liftIO (M.toList . _mUserStats <$> readTVarIO modelVar)
    return $ map (\(u, UserStats w l t _) -> UsersVg u w l t) stats

handleHome :: TVar Model -> Handler HomeData
handleHome modelVar = do
    m <- liftIO $ readTVarIO modelVar
    pure $ HomeData (m^.mResults, m^.mUserStats)

httpApp :: TVar Model -> Application
httpApp modelVar = serve (Proxy @ServerApi) (handleServerApi modelVar)

