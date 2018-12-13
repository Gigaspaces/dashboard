{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.Timeline where
import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client
import Data.Aeson
import GHC.Generics

-- :set -XOverloadedStrings


{-
http://hackage.haskell.org/package/servant-client
http://192.168.10.2:6060/api/timeline
{
   "Version":59,
   "name":"14.0.1-M4",
   "days":[
      {
         "name":"Planning",
         "total":129,
         "bottom":0,
         "top":129,
         "expected":129,
         "working_day":false
      },
      {
         "name":"Sun, Dec 9",
         "total":131,
         "bottom":0,
         "top":113,
         "expected":105.2,
         "working_day":true
      },
      {
         "name":"Mon, Dec 10",
         "total":137,
         "bottom":6,
         "top":103,
         "expected":85.4,
         "working_day":true
      },
      {
         "name":"Tue, Dec 11",
         "total":145,
         "bottom":14,
         "top":106,
         "expected":67.6,
         "working_day":true
      },
      {
         "name":"Wed, Dec 12",
         "total":150,
         "bottom":19,
         "top":96,
         "expected":46.8,
         "working_day":true
      },
      {
         "name":"Thu, Dec 13",
         "total":150,
         "bottom":19,
         "top":null,
         "expected":21,
         "working_day":true
      }
   ],
   "today":3
}

-}

data Day = Day
  {
    name :: Text
  , total :: Int
  , bottom :: Int
  , top :: Maybe Int
  , expected :: Double
  , working_day :: Bool
  } deriving (Show, Generic)
instance FromJSON Day


data Timeline = Timeline
  { version :: Int
  , timelineName :: Text
  , days :: [Day]
  , today :: Int
  } deriving (Show, Generic)

-- instance FromJSON Timeline
instance FromJSON Timeline where
  parseJSON = withObject "timeline" $ \o ->
    Timeline <$> o .: "Version" <*> o .: "name" <*> o .: "days"  <*> o .: "today"


type TimelineApi = "timeline" :> Get '[JSON] Timeline

timelineApi :: Proxy TimelineApi
timelineApi = Proxy

getTimeline :: ClientM Timeline
getTimeline = client timelineApi
