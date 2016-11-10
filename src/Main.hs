{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE FlexibleContexts    #-}

module Main where

import Control.Monad.Reader
import Control.Monad.Reader.Class
import Data.ByteString.Lazy (ByteString)
import Data.Char (digitToInt, isDigit, toLower)
import Data.List (sort)
import Data.List.Extra (nubOrd)
import Data.Text (Text)
import Data.Text.ICU (NormalizationMode (..), normalize)
import Options.Generic
import qualified Data.Aeson as Aeson (encode)
import qualified Data.ByteString.Lazy as BStr
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.Directory (createDirectoryIfMissing)

import Hyph_UTF8.Language
import Paths_hyph_utf8


main :: IO ()
main = do
  form :: ModeOpt <- getRecord "The Unicode normalization form to be used"
  createDirectoryIfMissing True $ "json-" ++ formPathSuffix (mode form)
  runReaderT go form
    where go = mapM_ (\lang -> write Pattern lang >> write Exception lang) tags


{- CLI option parsing -}

-- Fiant pupilli
deriving instance Generic NormalizationMode
deriving instance Read NormalizationMode

instance ParseField NormalizationMode
instance ParseFields NormalizationMode
instance ParseRecord NormalizationMode

newtype ModeOpt = ModeOpt { mode :: NormalizationMode }
  deriving (Show, Generic)

instance ParseRecord ModeOpt


{- Knuth-Liang pattern parsing and translation to text-score pairs -}

data KLPattern = Pattern | Exception
  deriving (Eq, Ord, Show, Bounded, Enum)

score :: KLPattern -> Text -> [Int]
score klp cs
  | T.null cs = [0]
  | scoring klp (T.head cs) = points klp : if T.null (T.tail cs)
                                              then []
                                              else score klp (T.tail $ T.tail cs)
  | otherwise = 0 : score klp (T.tail cs)
  where scoring Pattern = isDigit
        scoring Exception = (== '-')
        points Pattern = digitToInt (T.head cs)
        points Exception = 1


type KLPair = (Text, [Int])

pair :: MonadReader ModeOpt m => KLPattern -> Text -> m KLPair
pair klp t = do
  opt <- asks mode
  let t' = normalize opt t
  return (nonScoring klp t', score klp t')
  where nonScoring Pattern = T.filter (not . isDigit)
        nonScoring Exception = T.filter (/= '-')


{- JSON serialization, filepaths, and IO -}

parse :: MonadReader ModeOpt m => KLPattern -> Text -> m [KLPair]
parse klp t = mapM (pair klp) (T.lines t)

serialize :: MonadReader ModeOpt m => KLPattern -> Text -> m ByteString
serialize klp t = do
  patterns <- parse klp t
  let sortedUniques = (sort . nubOrd) patterns
  return (Aeson.encode sortedUniques)


klpFileName :: KLPattern -> String -> FilePath
klpFileName klp langTag = "hyph-" ++ langTag ++ extension klp
  where extension Pattern   = ".pat"
        extension Exception = ".hyp"

formPathSuffix :: NormalizationMode -> FilePath
formPathSuffix form = map toLower $ show form

destinationPath :: MonadReader ModeOpt m => KLPattern -> String -> m FilePath
destinationPath klp langTag = do
  form <- asks mode
  let path = "json-" ++ formPathSuffix form ++ "/" ++ klpFileName klp langTag ++ ".json"
  return path


readHyph :: KLPattern -> String -> IO Text
readHyph klp langTag = do
  file <- getDataFileName (klpFileName klp langTag ++ ".txt")
  ps <- T.IO.readFile file
  return ps

load :: (MonadIO m, MonadReader ModeOpt m) => KLPattern -> String -> m ByteString
load klp langTag = do
  patterns <- liftIO $ readHyph klp langTag
  jsonBlob <- serialize klp patterns
  return jsonBlob

write :: (MonadIO m, MonadReader ModeOpt m) => KLPattern -> String -> m ()
write klp langTag = do
  jsonBlob <- load klp langTag
  path <- destinationPath klp langTag
  liftIO $ BStr.writeFile path (BStr.snoc jsonBlob 0x0a)

