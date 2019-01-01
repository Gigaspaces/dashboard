{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}

module Client.Newman.Board where
import Data.Proxy
import Data.Text                 hiding (zipWith)
import Servant.API
import Servant.Client
import Data.Aeson
import GHC.Generics
import Client.Util
import Control.Monad (forM)
import Control.Monad.Reader

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
   failedTests :: Int,
   failed3TimesTests :: Int
 } deriving (Show, Generic)
instance FromJSON BuildStatus

data Build = Build
 {
   id :: Text,
   name :: Text,
   branch :: Text,
   buildStatus :: BuildStatus
 } deriving (Show, Generic)
instance FromJSON Build

newtype Dashboard = Dashboard
 {
   activeBuilds :: [Build]
 } deriving (Show, Generic)
instance FromJSON Dashboard

data Job = Job
 {
   jobName :: Text,
   submittedBy :: Text,
   totalTests :: Int,
   passedTests :: Int,
   jobFailedTests :: Int,
   runningTests :: Int
   } deriving (Show, Generic)

instance FromJSON Job where
    parseJSON = withObject "BuildJob" $ \o -> do
                              suite <- o .: "suite"
                              name <- suite .: "name"
                              submittedBy <- o .: "submittedBy"
                              totalTests <- o .: "totalTests"
                              passedTests <- o .: "passedTests"
                              failedTests <- o .: "failedTests"
                              runningTests <- o .: "runningTests"
                              return $ Job name submittedBy totalTests passedTests failedTests runningTests


data BuildAndJobs = BuildAndJobs
 {
    jobs :: [Job],
    build :: Maybe Build
 }  deriving (Show, Generic)

instance FromJSON BuildAndJobs where
    parseJSON = withObject "BuildJobs" $ \o -> do
                              jobs <- o .: "values"
                              return $ BuildAndJobs jobs Nothing

-- https://xap-newman:8443/api/newman/job?buildId=5c28f6a2b385942a71d888b1&all=true


type NewmanApi = BasicAuth "Newman's websites" User :> "dashboard" :> Get '[JSON] Dashboard
            :<|> BasicAuth "Newman's websites" User :> "job":> QueryParam "buildId" Text :> QueryParam "all" Text :> Get '[JSON] BuildAndJobs



newmanApi :: Proxy NewmanApi
newmanApi = Proxy

getDashboard :: BasicAuthData  -> ClientM Dashboard
getJobs :: BasicAuthData -> Maybe Text -> Maybe Text -> ClientM BuildAndJobs

(getDashboard :<|> getJobs) = client newmanApi

getBuildsAndJobs :: BasicAuthData -> ClientM [BuildAndJobs]
getBuildsAndJobs basicAuthData = do
  dashboard <- getDashboard basicAuthData
  let ids = fmap Client.Newman.Board.id (activeBuilds dashboard)
  buildAndJobs <- forM ids $ \id -> getJobs basicAuthData (Just id) (Just "true")
  return $ zipWith insertBuild (activeBuilds dashboard) buildAndJobs
 where
   insertBuild :: Build -> BuildAndJobs -> BuildAndJobs
   insertBuild b baj = baj{build = Just b}
