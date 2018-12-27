{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.Newman.Build where
import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client
import Data.Aeson
import GHC.Generics

import Servant.API.BasicAuth      (BasicAuthData (BasicAuthData))

data User = User
  { user :: Text
  , pass :: Text
  } deriving (Eq, Show)


-- :set -XOverloadedStrings


{-
http://hackage.haskell.org/package/servant-client
https://xap-newman.gspaces.com:8443/api/newman/jobs-view?limit=1&orderBy=-submitTime
-}

data BuildStatus = BuildStatus
 {
   totalTests:: Int,
   passedTests :: Int,
   failedTests :: Int,
   failed3TimesTests :: Int,
   runningTests :: Int,
   numOfTestRetries :: Int,
   totalJobs :: Int,
   pendingJobs :: Int,
   runningJobs :: Int,
   doneJobs :: Int,
   brokenJobs :: Int,
   suitesNames :: [Text],
   suitesIds :: [Text]
 } deriving (Show, Generic)
instance FromJSON BuildStatus

data Build = Build
 {
   id :: Text,
   name :: Text,
   branch :: Text,
   tags :: [Text],
   buildTime :: Int,
   buildStatus :: BuildStatus
 } deriving (Show, Generic)
instance FromJSON Build

newtype Dashboard = Dashboard
 {
   activeBuilds :: [Build]
 } deriving (Show, Generic)
instance FromJSON Dashboard

type NewmanApi = BasicAuth "Newman's websites" User :> "dashboard" :> Get '[JSON] Dashboard


newmanApi :: Proxy NewmanApi
newmanApi = Proxy

getDashboard :: BasicAuthData  -> ClientM Dashboard
getDashboard = client newmanApi
