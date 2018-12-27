{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where
import Data.Text
import Network.HTTP.Client (newManager, defaultManagerSettings, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings, newTlsManager, mkManagerSettings, newTlsManagerWith)
import Servant.Client
import Network.Connection         (TLSSettings(..))
import Servant.API.BasicAuth      (BasicAuthData (BasicAuthData))
import Client.Timeline
import Client.Newman.Build
import Client.Util (getBasicAuthData)

-- :set -XOverloadedStrings




getSecureManager :: IO Manager
getSecureManager = let tlsSettings = TLSSettingsSimple True False False -- Disable certificate verification completely
                   in newTlsManagerWith (mkManagerSettings tlsSettings Nothing)


main :: IO ()
main = do
  putStrLn ""
  authData <- getBasicAuthData "BasicAuthData.json"
  case authData of
    Nothing -> putStrLn "BasicAuthData.json not found"
    Just user -> do manager' <- newManager defaultManagerSettings
                    -- http://192.168.10.2:6060/api/timeline
                    -- res <- runClientM getTimeline (ClientEnv manager' (BaseUrl Http "192.168.10.2" 6060 "/api") Nothing)
                    res <- runClientM getTimeline (ClientEnv manager' (BaseUrl Http "192.168.10.2" 6060 "/api"))
                    case res of
                      Left err -> putStrLn $ "Error: " ++ show err
                      Right timeline -> print timeline
                    -- https://xap-newman.gspaces.com:8443/api/newman/
                    --  secureManager <- newManager tlsManagerSettings
                    putStrLn ""
                    secureManager <- getSecureManager
                    res <- runClientM (getDashboard user) (ClientEnv secureManager (BaseUrl Https "xap-newman.gspaces.com" 8443 "/api/newman"))
                    case res of
                      Left err -> putStrLn $ "Error: " ++ show err
                      Right dashboard -> print dashboard
