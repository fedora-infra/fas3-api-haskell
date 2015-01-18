{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fedora.FAS.Types.UTCTimeFAS where

import Control.Applicative
import Control.Lens (Iso', iso)
import Data.Aeson
import Data.Text (pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import System.Locale (defaultTimeLocale)

-- | This type is isomorphic to UTCTime, but we customize its FromJSON instance
-- to allow it to accept 0-offsets instead of only @Z@.
newtype UTCTimeFAS = UTCTimeFAS UTCTime deriving (Eq, Ord, ParseTime, FormatTime)

instance Show UTCTimeFAS where
  show (UTCTimeFAS u) = show u
  {-# INLINE show #-}

instance ToJSON UTCTimeFAS where
  toJSON (UTCTimeFAS t) = String (pack (take 23 str ++ "Z"))
    where
      str = formatTime defaultTimeLocale "%FT%T.%q" t
  {-# INLINE toJSON #-}

instance FromJSON UTCTimeFAS where
  parseJSON = withText "UTCTimeFAS"
              (\t -> parseUTCTime "%FT%T%QZ" t <|>
                     parseUTCTime "%FT%T%Q+00" t <|>
                     parseUTCTime "%FT%T%Q+00:00" t <|>
                     parseUTCTime "%FT%T%Q+0000" t)
    where
      parseUTCTime fmt t =
        case parseTime defaultTimeLocale fmt (unpack t) of
          Just d -> UTCTimeFAS <$> pure d
          _ -> fail "could not parse ISO-8601 date"
  {-# INLINE parseJSON #-}

utc :: Iso' UTCTimeFAS UTCTime
utc = iso (\(UTCTimeFAS t) -> t) UTCTimeFAS
{-# INLINE utc #-}
