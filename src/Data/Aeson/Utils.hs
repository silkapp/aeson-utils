{-# LANGUAGE CPP #-}

module Data.Aeson.Utils
  ( module Data.Aeson
  , module Data.Aeson.Types
  , eitherDecodeV
  , decodeV
  , fromFloatDigits
  , (.=?)
  , parseNumber
  ) where

import Data.Aeson
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Scientific
import Data.Text            (Text)
import Data.Attoparsec.Lazy (Result (..))
import qualified Data.Attoparsec.Lazy as Atto
import qualified Data.ByteString.Lazy as L

-- | Parsing

-- Allows atomic values on the top level

eitherDecodeV :: FromJSON a => L.ByteString -> Either String a
eitherDecodeV v = case Atto.parse value v of
  Fail _ _ err -> Left err
  Done _ r     -> case fromJSON r of
    Error e   -> Left e
    Success a -> Right a

decodeV :: FromJSON a => L.ByteString -> Maybe a
decodeV = either (const Nothing) Just . eitherDecodeV

-- | ToJSON

-- | Optionally create a Pair.
(.=?) :: ToJSON a => Text -> Maybe a -> Maybe Pair
k .=? v = fmap (k .=) v
{-# INLINE (.=?) #-}

-- | Convert a Scientific into an Integer if it doesn't have decimal points,
-- otherwise to a Double.
parseNumber :: Scientific -> Either Integer Double
parseNumber n
    | e >= 0    = Left $ c * 10 ^ e
    | otherwise = Right $ realToFrac n
  where
    e = base10Exponent n
    c = coefficient n
