{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}




module Client.Newman.Board where
import Data.Proxy
import Data.Text                 hiding (zipWith)
import Servant.API
import Servant.Client
import Data.Aeson
import GHC.Generics
import Client.Util               hiding (Event)
import Control.Monad (forM)
import Control.Monad.Reader
import Data.Sort
import Data.Maybe
import Servant.API.BasicAuth      (BasicAuthData (BasicAuthData))
import Debug.Trace


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
 } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Build = Build
 {
   id :: Text,
   name :: Text,
   branch :: Text,
   buildStatus :: BuildStatus
 } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)


newtype Dashboard = Dashboard
 {
   activeBuilds :: [Build]
 } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Job = Job
 {
   jobName :: Text,
   submittedBy :: Text,
   totalTests :: Int,
   passedTests :: Int,
   jobFailedTests :: Int,
   runningTests :: Int
 } deriving (Eq, Ord, Show, Generic, ToJSON)

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
 }  deriving (Eq, Show, Ord, Generic, ToJSON)

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




instance Diffable BuildAndJobs where
    key = maybe "" Client.Newman.Board.id . build
    deletedEvent = Ev "BuildAndJobs"
    createdEvent = Ev "BuildAndJobs"
    modifiedEvent = Ev "BuildAndJobs"
    diffables a = Diff <$> jobs a


instance Diffable Job where
    key = jobName
    deletedEvent = Ev "Job"
    createdEvent = Ev "Job"
    modifiedEvent = Ev "Job"
    diffables a = []



instance EventsGen BuildAndJobs where
    gen = generate

instance EventsGen Job where
    gen = generate


generate :: (Diffable a) => [a] -> [a] -> [Ev]
generate [] [] = undefined
generate [] l = createdEvent <$> l
generate before [] = deletedEvent <$> before
generate (a:rest1) (b:rest2) = if a == b
                               then generate rest1 rest2
                               else case compare (key a) (key b) of
                                      EQ -> generate rest1 rest2
                                      LT -> if sameKey a b
                                            then modifiedEvent b : generate rest1 rest2
                                            else deletedEvent a : generate rest1 (b:rest2)
                                      GT -> if sameKey a b
                                            then modifiedEvent b : generate rest1 rest2
                                            else createdEvent b : generate (a:rest1) rest2
           where
             sameKey b1 b2 = key b1 == key b2

{-
class Foo a where
    getAll :: a -> [Show]

instance Foo BuildAndJobs where
    getAll = jobs
-}
