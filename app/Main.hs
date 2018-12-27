{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text                                 hiding (filter)
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
import Network.WebSockets.Connection                  (defaultConnectionOptions, sendTextData, acceptRequest)
import Network.Wai.Handler.WebSockets                 (websocketsOr)
import qualified Network.WebSockets             as WS
import Network.WebSockets                             (ServerApp)
import Control.Monad                                  (forM_, forever)
import qualified Control.Exception              as Exception
import qualified Safe
import qualified Data.Maybe                     as Maybe
import qualified Control.Concurrent             as Concurrent

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


server :: State -> Server MyAPI
server state = return users1
    :<|> serveDirectoryWebApp "static-files"
    :<|> Tagged (websocketApp state)



type ClientId = Int
type Client   = (ClientId, WS.Connection)

newtype State = State {clients :: Concurrent.MVar [Client]}

app1 :: State -> Application
app1 state = serve myAPI (server state)


nextId :: [Client] -> ClientId
nextId = Maybe.maybe 0 (1 +) . Safe.maximumMay . fmap fst


connectClient :: Concurrent.MVar [Client] -> WS.Connection -> IO ClientId
connectClient clientRef conn = Concurrent.modifyMVar clientRef $ \clients -> do
  let clientId = nextId clients
  return ((clientId, conn) : clients, clientId)

withoutClient :: ClientId -> [Client] -> [Client]
withoutClient clientId = filter ((/=) clientId . fst)

disconnectClient ::  Concurrent.MVar [Client] -> ClientId -> IO ()
disconnectClient clientRef clientId = Concurrent.modifyMVar_ clientRef $ \clients ->
  return $ withoutClient clientId clients

broadcast :: Concurrent.MVar [Client] -> Text -> IO ()
broadcast clientsRef msg = do
  clients <- Concurrent.readMVar clientsRef
  forM_ clients $ \(_, conn) ->
      WS.sendTextData conn msg

websocketApp :: State -> Application
websocketApp state = websocketsOr defaultConnectionOptions (wsApp state) notAWebSocketRequestApp
  where
    wsApp :: State -> ServerApp
    wsApp state pending_conn = do
        conn <- acceptRequest pending_conn
        WS.forkPingThread conn 30
        clientId <- connect conn
--        sendTextData conn ("Hello, client!" :: Text)
        Exception.finally (forever (WS.receiveDataMessage conn))
                     (disconnect clientId)
          where
            disconnect :: ClientId -> IO ()
            disconnect = disconnectClient (clients state)
            connect :: WS.Connection -> IO ClientId
            connect = connectClient (clients state)
    notAWebSocketRequestApp :: Application
    notAWebSocketRequestApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

--type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived


main :: IO ()
main = do
    clients <- Concurrent.newMVar []
    run 8081 (app1 State{clients = clients})
