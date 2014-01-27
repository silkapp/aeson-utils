{-# LANGUAGE CPP #-}

module Data.Aeson.Utils
  ( module Data.Aeson
  , module Data.Aeson.Types
  , eitherDecodeV
  , decodeV
  , fromFloatDigits
  , (.=?)
  , withInteger
  , withParsedNumber
  , parseNumber
  , (.:*)
  , (.:?*)
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Parser (value)
import Data.Aeson.Types
import Data.Scientific
import Data.Text            (Text)
import Data.Attoparsec.Lazy (Result (..))
import qualified Data.Attoparsec.Lazy as Atto
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict  as H
#if ! MIN_VERSION_aeson(0,7,0)
import Data.Attoparsec.Number
#endif

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

-- | FromJSON

-- | Match on integer numbers
withInteger :: String -> (Integer -> Parser a) -> Value -> Parser a
withInteger err f v = withParsedNumber err f (const $ typeMismatch err v) v

-- | Match on numbers with one callback for Integers and one for Doubles.
withParsedNumber :: String -> (Integer -> Parser a) -> (Double -> Parser a) -> Value -> Parser a
withParsedNumber err f g v =
#if MIN_VERSION_aeson(0,7,0)
  case v of
    Number s -> case parseNumber s of
      Left  i -> f i
      Right d -> g d
    _ -> typeMismatch err v
#else
  flip (withNumber err) v $ \n -> case n of
    I i -> f i
    D d -> g d
#endif

-- | Convert a Scientific into an Integer if it doesn't have decimal points,
-- otherwise to a Double.
parseNumber :: Scientific -> Either Integer Double
parseNumber n
    | e >= 0    = Left $ c * 10 ^ e
    | otherwise = Right $ realToFrac n
  where
    e = base10Exponent n
    c = coefficient n

-- Lookup nested keys
-- Author: Petr Pudlák http://stackoverflow.com/a/18003411/182603

-- | Look up nested keys
(.:*) :: FromJSON a => Value -> [Text] -> Parser a
(.:*) v = parseJSON <=< foldM ((either fail return .) . lookupE) v

-- | Optionally look up nested keys
(.:?*) :: FromJSON a => Value -> [Text] -> Parser (Maybe a)
(.:?*) v ks = optional $ v .:* ks

lookupE :: Value -> Text -> Either String Value
lookupE o key = case o of
  Object obj -> case H.lookup key obj of
    Nothing -> Left $ "key " ++ show key ++ " not present"
    Just v  -> Right v
  _ -> Left $ "not an object"
