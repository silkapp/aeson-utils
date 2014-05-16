-- | This module provides a few small functions to make working with
-- aeson easier. Hopefully at some point they won't be needed anymore.
module Data.Aeson.Utils
  ( module Data.Aeson
  , module Data.Aeson.Types
  -- * Parsing values
  , decodeV
  , eitherDecodeV
  -- * Utilities
  , fromFloatDigits
  , (.=?)
  , parseNumber
  , floatingOrInteger
  ) where

import Data.Aeson
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Attoparsec.Lazy (Result (..))
import Data.Scientific
import Data.Text (Text)
import qualified Data.Attoparsec.Lazy as Atto
import qualified Data.ByteString.Lazy as L

-- * Parsing values

-- | Like 'decodeV', but returns an error message when decoding fails.
eitherDecodeV :: FromJSON a => L.ByteString -> Either String a
eitherDecodeV v = case Atto.parse value v of
  Fail _ _ err -> Left err
  Done _ r     -> case fromJSON r of
    Error e   -> Left e
    Success a -> Right a

-- | Deserialize any JSON value. Allows atomic values on the top level
decodeV :: FromJSON a => L.ByteString -> Maybe a
decodeV = either (const Nothing) Just . eitherDecodeV

-- * Utilities

-- | Optionally create a Pair.
(.=?) :: ToJSON a => Text -> Maybe a -> Maybe Pair
k .=? v = fmap (k .=) v
{-# INLINE (.=?) #-}

-- | Convert a Scientific into an Integer if it doesn't have decimal points,
-- otherwise to a Double.
parseNumber :: Scientific -> Either Integer Double
parseNumber = either Right Left . floatingOrInteger
{-# DEPRECATED parseNumber "Use Data.Scientific.floatingOrInteger instead" #-}
