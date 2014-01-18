{-# LANGUAGE CPP #-}

module Data.Aeson.Utils
  ( module Data.Aeson
  , module Data.Aeson.Types
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
import Data.Aeson.Types
import Data.Text (Text)
import Data.Scientific
import qualified Data.HashMap.Strict as H
#if ! MIN_VERSION_aeson(0,7,0)
import Data.Attoparsec.Number
#endif

-- | ToJSON

(.=?) :: ToJSON a => Text -> Maybe a -> Maybe Pair
k .=? v = fmap (k .=) v
{-# INLINE (.=?) #-}

-- | FromJSON

withInteger :: String -> (Integer -> Parser a) -> Value -> Parser a
withInteger err f v = withParsedNumber err f (const $ typeMismatch err v) v

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

parseNumber :: Scientific -> Either Integer Double
parseNumber n
    | e >= 0    = Left $ c * 10 ^ e
    | otherwise = Right $ realToFrac n
  where
    e = base10Exponent n
    c = coefficient n

-- Lookup nested keys
-- Author: Petr Pudlák http://stackoverflow.com/a/18003411/182603

(.:*) :: FromJSON a => Value -> [Text] -> Parser a
(.:*) v = parseJSON <=< foldM ((either fail return .) . lookupE) v

(.:?*) :: FromJSON a => Value -> [Text] -> Parser (Maybe a)
(.:?*) v ks = optional $ v .:* ks

lookupE :: Value -> Text -> Either String Value
lookupE o key = case o of
  Object obj -> case H.lookup key obj of
    Nothing -> Left $ "key " ++ show key ++ " not present"
    Just v  -> Right v
  _ -> Left $ "not an object"
