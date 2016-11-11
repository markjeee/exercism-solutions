-- Shamelessly stolen from:
-- https://www.cs.york.ac.uk/ftpdir/pub/haskell/contrib/Roman.hs

module Roman (numerals) where

import Data.Maybe (fromJust, fromMaybe)

numerals n = Just $ toRoman n

numH = [ ('I',   1), ('V',   5), ('X',  10), ('L',  50),
         ('C', 100), ('D', 500), ('M',1000) ]

subH = [ ('V','I'),  ('X','I'),  ('L','X'),
         ('C','X'),  ('D','C'),  ('M','C') ]

toRoman :: Int -> String
toRoman n = (reverse . snd) (foldr toNumeral (n,"") numH)

toNumeral st@(rdigit, base) (n,s)
  | n >= base = toNumeral st (n-base, rdigit:s)
  | n+k >= base = (n-base+k, rdigit:tdigit:s)
  | otherwise = (n,s)
  where tdigit = fromMaybe '\0' (lookup rdigit subH)
        k = fromMaybe 0 (lookup tdigit numH)
