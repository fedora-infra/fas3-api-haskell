{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Fedora.FAS.Types.Pages where

import Control.Applicative
import Control.Lens
import Control.Monad (mzero)
import Data.Aeson

data Pages = Pages {
    pagesCurrent :: Integer
  , pagesTotal   :: Integer
  } deriving (Eq, Ord, Show)

instance FromJSON Pages where
  parseJSON (Object v) = Pages
                         <$> v .:  "Current"
                         <*> v .:  "Total"
  parseJSON _          = mzero

makeFields ''Pages
