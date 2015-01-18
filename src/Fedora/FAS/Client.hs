{-# LANGUAGE OverloadedStrings #-}
module Fedora.FAS.Client (
  -- * Person
  getPerson
, getPeople

  -- * Group
, getGroups

  -- * Utility
, localClientConfig
) where

import Control.Exception as E
import Control.Lens
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Fedora.FAS.Types
import Network.HTTP.Types (urlEncode)
import Network.Wreq

localClientConfig :: APIKey -> ClientConfig
localClientConfig = ClientConfig "http://localhost:6543"
{-# INLINE localClientConfig #-}

-- TODO: This is inefficient.
encodePath :: String -> String
encodePath = C8.unpack . urlEncode False . C8.pack
{-# INLINE encodePath #-}

-- | Finds a unique person by some unique identifier ('SearchType').
--
-- Internally, this hits @\/api\/people\/<searchtype>\/<query>@.
getPerson :: ClientConfig -- ^ How to connect to FAS3
          -> SearchType -- ^ What to filter results by
          -> String -- ^ The search query
          -> IO (Either E.SomeException (Response PersonResponse))
getPerson (ClientConfig b a) search query = do
  let opts = defaults & param "apikey" .~ [a]
      url  = b ++ "/api/people/" ++ show search ++ "/" ++ encodePath query
  E.try $ asJSON =<< getWith opts url

-- | Get a list of something from the API.
getList :: FromJSON a
        => ClientConfig -- ^ How to connect to FAS3
        -> String -- ^ The URL (path) to hit
        -> Integer -- ^ The page number
        -> Integer -- ^ The limit
        -> IO (Either E.SomeException (Response a))
getList (ClientConfig b a) path page limit = do
  let opts = defaults & param "apikey" .~ [a]
                      & param "page" .~ [T.pack . show $ page]
                      & param "limit" .~ [T.pack . show $ limit]
      url = b ++ path
  E.try $ asJSON =<< getWith opts url

-- | Get a list of all people.
--
-- Internally, this hits @\/api\/people@.
getPeople :: ClientConfig -- ^ How to connect to FAS3
          -> Integer -- ^ The page number
          -> Integer -- ^ The limit
          -> IO (Either E.SomeException (Response PeopleResponse))
getPeople = getList ?? "/api/people"
{-# INLINE getPeople #-}

-- | Get a list of all groups.
--
-- Internally, this hits @\/api\/group@.
getGroups :: ClientConfig -- ^ How to connect to FAS3
          -> Integer -- ^ The page number
          -> Integer -- ^ The limit
          -> IO (Either E.SomeException (Response GroupsResponse))
getGroups = getList ?? "/api/group"
{-# INLINE getGroups #-}
