{-# LANGUAGE OverloadedStrings #-}
module Fedora.FAS.Types.Person where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Fedora.FAS.Types.Pages
import Fedora.FAS.Types.UTCTimeFAS

data Person = Person {
    personUsername     :: String
  , personStatus       :: Integer
  , personIdNumber     :: Integer
  , personAvatar       :: Maybe String
  , personFullname     :: String
  , personCreationDate :: UTCTimeFAS
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

data PersonResponse = PersonResponse {
    personResponseStartTimestamp :: UTCTimeFAS
  , personResponseEndTimestamp   :: UTCTimeFAS
  , personResponsePeople         :: Person
  } deriving (Eq, Ord, Show)

instance FromJSON PersonResponse where
  parseJSON (Object v) = PersonResponse
                         <$> v .:  "StartTimeStamp"
                         <*> v .:  "EndTimeStamp"
                         <*> v .:  "People"
  parseJSON _          = mzero

data PeopleResponse = PeopleResponse {
    poepleResponseStartTimestamp :: UTCTimeFAS
  , peopleResponseEndTimestamp   :: UTCTimeFAS
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
