{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text
import Servant.API

import Lib


import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import GHC.Generics
import qualified Data.Aeson.Parser

import Data.Time.Calendar
import Servant
import Network.Wai
import Network.Wai.Handler.Warp


type MyAPI = "users" :> Get '[JSON] [User]
    :<|> "static" :> Raw

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]


myAPI :: Proxy MyAPI
myAPI = Proxy


server :: Server MyAPI
server = return users1
    :<|> serveDirectoryWebApp "static-files"

app1 :: Application
app1 = serve myAPI server

main :: IO ()
main = run 8081 app1
