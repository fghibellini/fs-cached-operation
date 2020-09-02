{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CachedOperation
 ( runCachedOperation
 , OperationResult(..)
 , PathSafe(..)
 ) where

import Data.Aeson
import Data.Functor
import Data.Text
import System.Directory
import System.FilePath
import Control.Exception
import Control.Monad

data CachedResult a
  = CachedResultOk a
  | CachedResultPermanentFailure

instance ToJSON a => ToJSON (CachedResult a) where
  toJSON (CachedResultOk x) = object [ "type" .= String "Success", "value" .= toJSON x ]
  toJSON CachedResultPermanentFailure = object [ "type" .= String "PermanentFailure" ]

instance FromJSON a => FromJSON (CachedResult a) where
  parseJSON = withObject "CachedResult" $ \o -> do
    t <- o .: "type"
    case (t :: Text) of
      "Success" -> do
        v <- o .: "value"
        CachedResultOk <$> parseJSON v
      "PermanentFailure" -> pure CachedResultPermanentFailure
      _ -> fail "Invalid type"

-- | Types that can be used to generate safe filenames.
-- Typically this will be the key of your cache.
class PathSafe a where
  toPathPart :: a -> FilePath

instance PathSafe String where
  toPathPart = id

instance PathSafe Text where
  toPathPart = unpack

data OperationResult a
  = Ok a
  | PermanentFailure
  | TemporaryFailure

runCachedOperation :: forall a b . (PathSafe a, ToJSON b, FromJSON b) => FilePath -> (a -> IO (OperationResult b)) -> a -> IO b
runCachedOperation dir f x = do
  let cacheFile = dir </> toPathPart x
  cacheExists <- doesFileExist cacheFile
  if cacheExists
  then (eitherDecodeFileStrict' cacheFile) <&!> (\case
    Right (CachedResultOk x) -> x
    Right CachedResultPermanentFailure -> error "Could not resolve: Request failed"
    Left msg -> error ("Could not resolve: " <> msg))
  else do
    res <- try $ f x
    case res of
      Right (Ok x) -> do
        encodeFile cacheFile (CachedResultOk x)
        pure x
      Right PermanentFailure -> do
        encodeFile cacheFile (CachedResultPermanentFailure :: CachedResult b)
        error "Permanent failure"
      Right TemporaryFailure -> do
        error "Temporary failure"
      Left (_ :: SomeException) -> do
        error "Temporary failure"

x <&!> f = f <$!> x
