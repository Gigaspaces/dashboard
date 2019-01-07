{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module Client.Util (getBasicAuthData, Diffable(..), Event(..), Ev(..), toText, Diff(..), EventsGen(..)) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Encode.Pretty
import Data.Text hiding      (empty)
import Data.Text.Encoding    (encodeUtf8, decodeUtf8)
import Servant.API.BasicAuth (BasicAuthData(..))
import GHC.Generics
import qualified Data.HashMap.Lazy as HML        ( lookup )
import qualified Data.ByteString.Lazy.Char8 as LC8  (unpack)
import Control.Applicative                       ( empty, pure )

instance FromJSON BasicAuthData where
     parseJSON = withObject "basicAuthData" $ \o -> do
                         name  <- o .: "name"
                         password  <- o .: "password"
                         return $ BasicAuthData (encodeUtf8 name) (encodeUtf8 password)




getBasicAuthData :: String -> IO (Maybe BasicAuthData)
getBasicAuthData location = decode <$> B.readFile location


toText ::(ToJSON a) => a -> Text
toText = decodeUtf8 . B.toStrict . encodePretty

-- =============================================

data Event a = DeletedEvent Text
             | CreatedEvent a
             | ModifiedEvent a
  deriving (Eq, Show)

instance (FromJSON a, Eq a, Show a) => FromJSON (Event a) where
        parseJSON (Object o) = case (HML.lookup (pack "type") o, HML.lookup (pack "value") o) of
                                  (Just (String "Deleted"), Just (String v)) ->  return (DeletedEvent v)
                                  (Just (String "Created"), Just (Object v)) ->  CreatedEvent <$> parseJSON (Object v)
                                  (Just (String "Modified"), Just (Object v)) -> ModifiedEvent <$> parseJSON (Object v)
                                  _ -> empty

instance (ToJSON a, Eq a, Show a) => ToJSON (Event a) where
    toJSON (DeletedEvent t) = object [ "type" .= String "Deleted"
                                       , "value" .= String t ]
    toJSON (CreatedEvent a) = object [ "type" .= String "Created"
                                       , "value" .= toJSON a ]
    toJSON (ModifiedEvent a) = object [ "type" .= String "Modified"
                                       , "value" .= toJSON a ]
{-
*Main Client.Newman.Board Data.Aeson> toJSON (DeletedEvent "foo")
Object (fromList [("key",String "foo"),("type",String "Deleted")])

*Main Data.Aeson> (decode (encode (CreatedEvent (BuildStatus 10 10)))) ::  Maybe (Event BuildStatus)
Just (CreatedEvent (BuildStatus {failedTests = 10, failed3TimesTests = 10}))

-}

class Eq a => Diffable a where
    key :: a -> Text
    deletedEvent :: a -> Ev
    createdEvent :: a -> Ev
    modifiedEvent :: a -> Ev
    diffables :: a -> [Diff]

data Ev = forall s. (Show s, ToJSON s) => Ev String s
data Diff = forall d. (Diffable d) => Diff d

instance Show Ev where
    show (Ev tag s) = "{\"object\":" ++ "\"" ++ tag ++ "\", " ++  LC8.unpack (encode s) ++ "}"

{-
 Ev "BuildStatus" (BuildStatus 10 10)
-}

class EventsGen a where
    gen :: [a] -> [a] -> [Ev]
