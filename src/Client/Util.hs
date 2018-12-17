{-# LANGUAGE OverloadedStrings     #-}
module Client.Util (getBasicAuthData) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Data.Text
import Data.Text.Encoding    (encodeUtf8)
import Servant.API.BasicAuth (BasicAuthData(..))


instance FromJSON BasicAuthData where
     parseJSON = withObject "basicAuthData" $ \o -> do
                         name  <- o .: "name"
                         password  <- o .: "password"
                         return $ BasicAuthData (encodeUtf8 name) (encodeUtf8 password)




getBasicAuthData :: String -> IO (Maybe BasicAuthData)
getBasicAuthData location = decode <$> B.readFile location
