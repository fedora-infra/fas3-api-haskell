module Fedora.FAS.Types.ClientConfig (
  ClientConfig (..)
, localClientConfig
) where

import qualified Data.Text as T

type APIKey = T.Text

data ClientConfig = ClientConfig {
    baseUrl :: String
  , apiKey  :: APIKey
  } deriving (Eq, Ord, Show)

-- | A 'ClientConfig' which is configured to use the default FAS3 development
-- server (at @http:\/\/localhost:6543\/@).
localClientConfig :: APIKey -> ClientConfig
localClientConfig = ClientConfig "http://localhost:6543"
{-# INLINE localClientConfig #-}
