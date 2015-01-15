{-# LANGUAGE TemplateHaskell #-}
module Fedora.FAS.Types where

import Control.Lens

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
