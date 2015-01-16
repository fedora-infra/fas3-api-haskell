{-# LANGUAGE OverloadedStrings #-}
module Fedora.FAS.Client where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8
import Fedora.FAS.Types
import Network.Wreq
--import Network.Wreq.Session

localClientConfig :: APIKey -> ClientConfig
localClientConfig = ClientConfig "http://localhost:6543"

listPeople :: ClientConfig -- ^ How to connect to FAS3
           -> SearchType -- ^ What to filter results by
           -> String -- ^ The search query
           -> IO C8.ByteString -- TODO
listPeople (ClientConfig b a) search query = do
  let opts = defaults & param "apikey" .~ [a]
  r <- getWith opts b
  return $ r ^. responseBody
