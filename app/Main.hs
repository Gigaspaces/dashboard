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

import Network.HTTP.Types
import Network.WebSockets.Connection (defaultConnectionOptions, sendTextData, acceptRequest)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp)

type MyAPI = "users" :> Get '[JSON] [User]
    :<|> "static" :> Raw
    :<|> "websocket" :> Raw

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
    :<|> websocketfunc



app1 :: Application
app1 = serve myAPI server


websocketfunc :: Tagged Handler Application
websocketfunc = Tagged $ app2

--websocketfunc = Tagged $ return (\respond  ->  respond helloworld')
--      where helloworld' = responseLBS
--                               status200
--                               [("Content-Type", "text/plain")]
--                               "Hello, Web!"


app2 :: Application
app2 = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        sendTextData conn ("Hello, client!" :: Text)

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived


main :: IO ()
main = run 8081 app1
