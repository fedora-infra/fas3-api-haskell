{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Fedora.FAS.Types where

import Control.Applicative
import Control.Lens
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

type APIKey = T.Text

data ClientConfig = ClientConfig {
    baseUrl :: String
  , apiKey  :: APIKey
  } deriving (Eq, Ord, Show)

data Person = Person {
    personUsername     :: String
  , personStatus       :: Integer
  , personIdNumber     :: Integer
  , personAvatar       :: Maybe String
  , personFullname     :: String
  , personCreationDate :: UTCTime
  , personIrcNick      :: Maybe String
  , personEmail        :: String
  } deriving (Eq, Ord, Show)

instance FromJSON Person where
  parseJSON (Object v) = Person
                         <$> v .:  "Username"
                         <*> v .:  "Status"
                         <*> v .:  "PeopleId"
                         <*> v .:? "Avatar"
                         <*> v .:  "Fullname"
                         <*> v .:  "CreationDate"
                         <*> v .:? "Ircnick"
                         <*> v .:  "Email"
  parseJSON _          = mzero

makeLenses ''Person

data PersonResponse = PersonResponse {
    prStartTimestamp :: UTCTime
  , prEndTimestamp   :: UTCTime
  , prPeople         :: [Person]
  } deriving (Eq, Ord, Show)

makeLenses ''PersonResponse

data SearchType = Id | Username | Email | IRCNick deriving (Eq, Ord)

instance Show SearchType where
  show Id       = "id"
  show Username = "username"
  show Email    = "email"
  show IRCNick  = "ircnick"
