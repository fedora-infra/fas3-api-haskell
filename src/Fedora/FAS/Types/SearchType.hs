module Fedora.FAS.Types.SearchType where

-- | When performing a search for a unique person, what should we filter on?
data PersonSearchType = Id | Username | Email | IRCNick deriving (Eq, Ord)

instance Show PersonSearchType where
  show Id       = "id"
  show Username = "username"
  show Email    = "email"
  show IRCNick  = "ircnick"

-- | When performing a search for a unique group, what should we filter on?
data GroupSearchType = GroupId | GroupName deriving (Eq, Ord)

instance Show GroupSearchType where
  show GroupId       = "id"
  show GroupName     = "name"
