{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Fedora.FAS.Lenses where

import Control.Lens
import Fedora.FAS.Types

makeFields ''Group
makeFields ''GroupResponse
makeFields ''GroupsResponse
makeFields ''Person
makeFields ''PersonResponse
makeFields ''PeopleResponse
