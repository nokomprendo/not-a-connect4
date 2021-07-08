{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module NaC4.Server.HttpApp where

import Data.Aeson
import GHC.Generics
import Lucid
import Servant
import Servant.HTML.Lucid

type ApiRoute = "api" :> Get '[JSON] [Person]
type HomeRoute = Get '[HTML] [Person]

type ServerApi
    =    ApiRoute
    :<|> HomeRoute

handleServerApi :: Server ServerApi
handleServerApi
    =    handlePersons
    :<|> handlePersons

handlePersons :: Handler [Person]
handlePersons = pure people

httpApp :: Application
httpApp = serve (Proxy @ServerApi) handleServerApi

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

