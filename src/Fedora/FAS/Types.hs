{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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

makeFields ''Person

data PersonResponse = PersonResponse {
    personResponseStartTimestamp :: UTCTime
  , personResponseEndTimestamp   :: UTCTime
  , personResponsePeople         :: Person
  } deriving (Eq, Ord, Show)

instance FromJSON PersonResponse where
  parseJSON (Object v) = PersonResponse
                         <$> v .:  "StartTimeStamp"
                         <*> v .:  "EndTimeStamp"
                         <*> v .:  "People"
  parseJSON _          = mzero

makeFields ''PersonResponse

data Pages = Pages {
    pagesCurrent :: Integer
  , pagesTotal   :: Integer
  } deriving (Eq, Ord, Show)

instance FromJSON Pages where
  parseJSON (Object v) = Pages
                         <$> v .:  "Current"
                         <*> v .:  "Total"
  parseJSON _          = mzero

makeFields ''Pages

data PeopleResponse = PeopleResponse {
    poepleResponseStartTimestamp :: UTCTime
  , peopleResponseEndTimestamp   :: UTCTime
  , peopleResponsePages          :: Pages
  , peopleResponsePeople         :: [Person]
  } deriving (Eq, Ord, Show)

instance FromJSON PeopleResponse where
  parseJSON (Object v) = PeopleResponse
                         <$> v .:  "StartTimeStamp"
                         <*> v .:  "EndTimeStamp"
                         <*> v .:  "Pages"
                         <*> v .:  "People"
  parseJSON _          = mzero

makeFields ''PeopleResponse

data SearchType = Id | Username | Email | IRCNick deriving (Eq, Ord)

instance Show SearchType where
  show Id       = "id"
  show Username = "username"
  show Email    = "email"
  show IRCNick  = "ircnick"
