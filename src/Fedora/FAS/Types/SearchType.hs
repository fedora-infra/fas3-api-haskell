module Fedora.FAS.Types.SearchType where

-- | When performing a search for a unique person, what should we filter on?
data SearchType = Id | Username | Email | IRCNick deriving (Eq, Ord)

instance Show SearchType where
  show Id       = "id"
  show Username = "username"
  show Email    = "email"
  show IRCNick  = "ircnick"
