{-# LANGUAGE OverloadedStrings #-}
module Fedora.FAS.Types.Group where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Fedora.FAS.Types.Pages
import Fedora.FAS.Types.UTCTimeFAS

data Group = Group {
    groupDisplayName    :: Maybe String
  , groupName           :: String
  , groupUrl            :: Maybe String
  , groupNeedApproval   :: Bool
  , groupMailingListUrl :: Maybe String
  , groupSelfRemoval    :: Bool
  , groupIsPrivate      :: Bool
  , groupIsInviteOnly   :: Bool
  , groupGroupType      :: String -- TODO: sum type?
  , groupPicture        :: Maybe String
  , groupMailingList    :: Maybe String
  , groupIrcChannel     :: Maybe String
  , groupIrcNetwork     :: Maybe String
  , groupOwner          :: String
  , groupCreationDate   :: UTCTimeFAS
  } deriving (Eq, Ord, Show)

instance FromJSON Group where
  parseJSON (Object v) = Group
                         <$> v .:? "DisplayName"
                         <*> v .:  "Name"
                         <*> v .:? "Url"
                         <*> v .:  "NeedApproval"
                         <*> v .:? "MailingListUrl"
                         <*> v .:  "SelfRemoval"
                         <*> v .:  "IsPrivate"
                         <*> v .:  "IsInviteOnly"
                         <*> v .:  "GroupType"
                         <*> v .:? "Picture"
                         <*> v .:? "MailingList"
                         <*> v .:? "IrcChannel"
                         <*> v .:? "IrcNetwork"
                         <*> v .:  "Owner"
                         <*> v .:  "CreationDate"
  parseJSON _          = mzero

data GroupResponse = GroupResponse {
    groupResponseStartTimestamp :: UTCTimeFAS
  , groupResponseEndTimestamp   :: UTCTimeFAS
  , groupResponseGroups         :: Group
  } deriving (Eq, Ord, Show)

instance FromJSON GroupResponse where
  parseJSON (Object v) = GroupResponse
                         <$> v .:  "StartTimeStamp"
                         <*> v .:  "EndTimeStamp"
                         <*> v .:  "Group"
  parseJSON _          = mzero

data GroupsResponse = GroupsResponse {
    groupsResponseStartTimestamp :: UTCTimeFAS
  , groupsResponseEndTimestamp   :: UTCTimeFAS
  , groupsResponsePages          :: Pages
  , groupsResponseGroups         :: [Group]
  } deriving (Eq, Ord, Show)

instance FromJSON GroupsResponse where
  parseJSON (Object v) = GroupsResponse
                         <$> v .:  "StartTimeStamp"
                         <*> v .:  "EndTimeStamp"
                         <*> v .:  "Pages"
                         <*> v .:  "Groups"
  parseJSON _          = mzero
