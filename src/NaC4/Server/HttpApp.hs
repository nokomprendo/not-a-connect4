{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module NaC4.Server.HttpApp where

import NaC4.Server.Model

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import GHC.Generics
import Lucid
import Servant
import Servant.HTML.Lucid

type ApiRoute = "api" :> Get '[JSON] [Person]
type HomeRoute = Get '[HTML] [T.Text]

type ServerApi
    =    ApiRoute
    :<|> HomeRoute

handleServerApi :: IORef Model -> Server ServerApi
handleServerApi modelRef
    =    handlePersons
    :<|> handleHome modelRef

handlePersons :: Handler [Person]
handlePersons = pure people

handleHome :: IORef Model -> Handler [T.Text]
handleHome modelRef = 
    liftIO (map fst . M.toList . _clients <$> readIORef modelRef)

httpApp :: IORef Model -> Application
httpApp modelRef = serve (Proxy @ServerApi) (handleServerApi modelRef)


-- HTML serialization of a list of persons
instance ToHtml [T.Text] where
  toHtml cs = ul_ $ do
    forM_ cs $ \c -> li_ $ toHtml c

  toHtmlRaw = toHtml



data Person = Person
  { firstName :: String
  , lastName  :: String
  } deriving Generic -- for the JSON instance

instance ToJSON Person

instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName person)

  -- do not worry too much about this
  toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"

    -- this just calls toHtml on each person of the list
    -- and concatenates the resulting pieces of HTML together
    foldMap toHtml persons

  toHtmlRaw = toHtml

people :: [Person]
people =
  [ Person "Isaac"  "Newton"
  , Person "Albert" "Einstein"
  ]

