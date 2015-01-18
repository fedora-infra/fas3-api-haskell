{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Fedora.FAS.Types.ClientConfig where

import qualified Data.Text as T

type APIKey = T.Text

data ClientConfig = ClientConfig {
    baseUrl :: String
  , apiKey  :: APIKey
  } deriving (Eq, Ord, Show)

