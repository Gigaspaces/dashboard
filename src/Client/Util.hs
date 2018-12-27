{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Client.Util (getBasicAuthData, EventType(..), Event(..), EventSource(..)) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Data.Text
import Data.Text.Encoding    (encodeUtf8)
import Servant.API.BasicAuth (BasicAuthData(..))
import GHC.Generics


instance FromJSON BasicAuthData where
     parseJSON = withObject "basicAuthData" $ \o -> do
                         name  <- o .: "name"
                         password  <- o .: "password"
                         return $ BasicAuthData (encodeUtf8 name) (encodeUtf8 password)




getBasicAuthData :: String -> IO (Maybe BasicAuthData)
getBasicAuthData location = decode <$> B.readFile location

data EventType = Deleted | Created | Modified Text
                 deriving (Show, Generic, Eq)
instance FromJSON EventType

class (Show a, FromJSON a) => Event a where
    eventName :: a -> Text
    eventType :: a -> EventType


class (Event b) => EventSource a b | a -> b where
    events :: a -> a -> [b]
