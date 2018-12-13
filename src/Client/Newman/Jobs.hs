{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.Newman.Jobs where
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
{
   "values":[
      {
         "id":"5c115dfdb385944809514548",
         "buildId":"5c1126cbb385944809511d6a",
         "buildName":"20204",
         "buildBranch":"14.0.1-m4-build",
         "suiteId":"59f25d19b3859424cac590bf",
         "suiteName":"xap-core",
         "jobConfigId":"5b4c9342b3859411ee82c265",
         "jobConfigName":"ORACLE_8",
         "submitTime":1544642045253,
         "startTime":1544661472884,
         "endTime":1544665814596,
         "testURI":null,
         "submittedBy":"root",
         "state":"DONE",
         "totalTests":3658,
         "passedTests":3658,
         "failedTests":0,
         "failed3TimesTests":0,
         "runningTests":0,
         "numOfTestRetries":0,
         "preparingAgents":[

         ]
      }
   ],
   "offset":0,
   "limit":1,
   "back":null,
   "self":"https://xap-newman.gspaces.com:8443/api/newman/jobs-view?orderBy=-submitTime&offset=0&limit=1",
   "next":"https://xap-newman.gspaces.com:8443/api/newman/jobs-view?orderBy=-submitTime&offset=1&limit=1"
}
-}

data Job = Job
  {
    id :: Text
  , buildId :: Text
  , buildName :: Text
  , buildBranch :: Text
  , suiteId :: Text
  , suiteName :: Maybe Text
  , jobConfigId :: Maybe Text
  , jobConfigName :: Maybe Text
  , submitTime :: Maybe Integer
  , startTime :: Maybe Integer
  , submittedBy :: Maybe Text
  , state :: Text
  , totalTests :: Int
  , passedTests :: Int
  , failedTests :: Int
  , failed3TimesTests :: Int
  , runningTests :: Int
  , numOfTestRetries :: Int
  } deriving (Show, Generic)
instance FromJSON Job

data Jobs = Jobs
 {
     values :: [Job]
   , offset :: Int
   , limit :: Int
   , back :: Maybe Text
   , self :: Text
   , next :: Maybe Text
 } deriving (Show, Generic)
instance FromJSON Jobs

{-
[
   {
      "id":"5b4c9342b3859411ee82c265",
      "name":"ORACLE_8",
      "javaVersion":"ORACLE_8_45"
   },
   {
      "id":"5b4c9825b3859411ee82c2f3",
      "name":"ORACLE_9",
      "javaVersion":"ORACLE_9_0_4"
   },
   {
      "id":"5bf160bb1f31eb789fc0fa65",
      "name":"OPENJDK_11",
      "javaVersion":"OPENJDK_11_0_1"
   }
]
-}

data JobConfig = JobConfig
 {
     jobId :: Text
   , name :: Maybe Text
   , javaVersion :: Maybe Text
 } deriving (Show)

instance FromJSON JobConfig where
  parseJSON = withObject "jobConfig" $ \o ->
    JobConfig <$> o .: "id" <*> o .: "name" <*> o .: "javaVersion"



-- jobs-view?limit=1&orderBy=-submitTime
type NewmanApi = BasicAuth "Newman's websites" User :> "jobs-view" :> QueryParam "limit" Int :> QueryParam "orderBy" Text :> Get '[JSON] Jobs
            :<|> BasicAuth "Newman's websites" User :> "job-config" :> Get '[JSON] [JobConfig]



newmanApi :: Proxy NewmanApi
newmanApi = Proxy

getJobs :: BasicAuthData -> Maybe Int -> Maybe Text -> ClientM Jobs
getJobsConfig :: BasicAuthData  -> ClientM [JobConfig]


(getJobs :<|> getJobsConfig) = client newmanApi
