{-# LANGUAGE OverloadedStrings #-}
module Fedora.FAS.Client (
  -- * Person
  getPerson
, getPeople

  -- * Group
, getGroup
, getGroups

  -- * Utility
, localClientConfig
, runFasQuery
) where

import Control.Exception as E
import Control.Lens
import Control.Monad.Reader
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import Fedora.FAS.Types
import Network.HTTP.Types (urlEncode)
import Network.Wreq

-- TODO: This is inefficient.
encodePath :: String -> String
encodePath = C8.unpack . urlEncode False . C8.pack
{-# INLINE encodePath #-}

runFasQuery :: ReaderT r m a -> r -> m a
runFasQuery = runReaderT

-- | Get a list of something from the API.
getList :: FromJSON a
        => String -- ^ The URL (path) to hit
        -> Integer -- ^ The page number
        -> Integer -- ^ The limit
        -> ReaderT ClientConfig IO (Either E.SomeException (Response a))
getList path page limit = do
  (ClientConfig b a) <- ask
  let opts = defaults & param "apikey" .~ [a]
                      & param "page" .~ [T.pack . show $ page]
                      & param "limit" .~ [T.pack . show $ limit]
      url = b ++ path
  lift $ E.try $ asJSON =<< getWith opts url

-- | Finds a unique person by some unique identifier ('SearchType').
--
-- Internally, this hits @\/api\/people\/<searchtype>\/<query>@.
getPerson :: PersonSearchType -- ^ What to filter results by
          -> String -- ^ The search query
          -> ReaderT ClientConfig IO (Either E.SomeException (Response PersonResponse))
getPerson search query = do
  (ClientConfig b a) <- ask
  let opts = defaults & param "apikey" .~ [a]
      url  = b ++ "/api/people/" ++ show search ++ "/" ++ encodePath query
  lift $ E.try $ asJSON =<< getWith opts url

-- | Get a list of all people.
--
-- Internally, this hits @\/api\/people@.
getPeople :: Integer -- ^ The page number
          -> Integer -- ^ The limit
          -> ReaderT ClientConfig IO (Either E.SomeException (Response PeopleResponse))
getPeople = getList "/api/people"
{-# INLINE getPeople #-}

-- | Finds a unique group by some unique identifier ('GroupSearchType').
--
-- Internally, this hits @\/api\/groups\/<searchtype>\/<query>@.
getGroup :: GroupSearchType -- ^ What to filter results by
          -> String -- ^ The search query
          -> ReaderT ClientConfig IO (Either E.SomeException (Response PersonResponse))
getGroup search query = do
  (ClientConfig b a) <- ask
  let opts = defaults & param "apikey" .~ [a]
      url  = b ++ "/api/groups/" ++ show search ++ "/" ++ encodePath query
  lift $ E.try $ asJSON =<< getWith opts url

-- | Get a list of all groups.
--
-- Internally, this hits @\/api\/group@.
getGroups :: Integer -- ^ The page number
          -> Integer -- ^ The limit
          -> ReaderT ClientConfig IO (Either E.SomeException (Response GroupResponse))
getGroups = getList "/api/group"
{-# INLINE getGroups #-}
