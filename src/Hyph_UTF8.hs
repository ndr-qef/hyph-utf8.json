{-# LANGUAGE OverloadedStrings #-}

module Hyph_UTF8
    ( module Hyph_UTF8.Language
    , KLPattern (..)
    , write
    ) where

import Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import Data.Char (digitToInt, isDigit)
import Data.List.Extra (nubOrd)
import Data.Text (Text)
import Data.Text.ICU (NormalizationMode (..), normalize)
import qualified Data.ByteString.Lazy as BStr
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Hyph_UTF8.Language

import Paths_hyph_utf8


data KLPattern = Pattern | Exception deriving (Eq,Ord,Show,Bounded,Enum)


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

pair :: KLPattern -> Text -> KLPair
pair klp t = (normalize NFC $ nonScoring klp t, score klp t)
  where nonScoring Pattern = T.filter (not . isDigit)
        nonScoring Exception = T.filter (/= '-')

parse :: KLPattern -> Text -> [KLPair]
parse klp t = map (pair klp) (T.lines t)

serialize :: KLPattern -> Text -> ByteString
serialize klp = A.encode . (nubOrd . (parse klp))


extension :: KLPattern -> String
extension Pattern = ".pat"
extension Exception = ".hyp"

klpFileName :: KLPattern -> String -> FilePath
klpFileName klp langTag = "hyph-" ++ langTag ++ extension klp

load :: KLPattern -> String -> IO ByteString
load klp langTag = do
  ps <- getDataFileName (klpFileName klp langTag ++ ".txt") >>= T.IO.readFile
  let jsonBlob = serialize klp ps
  return jsonBlob

write :: KLPattern -> String -> IO ()
write klp langTag = do
    jsonBlob <- load klp langTag
    BStr.writeFile ("json/" ++ klpFileName klp langTag ++ ".json") (BStr.snoc jsonBlob 0x0a)
