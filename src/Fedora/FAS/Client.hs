{-# LANGUAGE OverloadedStrings #-}
module Fedora.FAS.Client where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import Fedora.FAS.Types
import Network.HTTP.Types (urlEncode)
import Network.Wreq

localClientConfig :: APIKey -> ClientConfig
localClientConfig = ClientConfig "http://localhost:6543"

-- TODO: This is inefficient.
encodePath :: String -> String
encodePath = C8.unpack . urlEncode False . C8.pack

-- | Finds a unique person by some unique identifier ('SearchType').
--
-- Internally, this hits @/api/people/<searchtype>/<query>@.
getPerson :: ClientConfig -- ^ How to connect to FAS3
          -> SearchType -- ^ What to filter results by
          -> String -- ^ The search query
          -> IO (Maybe PersonResponse)
getPerson (ClientConfig b a) search query = do
  let opts = defaults & param "apikey" .~ [a]
  r <- getWith opts (b ++ "/api/people/" ++ show search ++ "/" ++ encodePath query)
  LC8.putStrLn $ r ^. responseBody
  return . decode $ r ^. responseBody
