{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Fedora.FAS.Types where

import Control.Applicative
import Control.Lens
import Control.Monad (mzero)
import Data.Aeson

data Person = Person {
    personUsername     :: String
  , personStatus       :: Integer
  , personIdNumber     :: Integer
  , personAvatar       :: Maybe String
  , personFullname     :: String
  , personCreationDate :: String -- TODO: Use a real Date type
  , personIrcNick      :: Maybe String
  , personEmail        :: String
  } deriving (Eq, Ord, Show)

makeLenses ''Person

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
