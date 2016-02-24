module Main where

import Control.Monad

import Hyph_UTF8


main :: IO ()
main = do
  mapM_ (\lang -> write Pattern lang >> write Exception lang) tags
