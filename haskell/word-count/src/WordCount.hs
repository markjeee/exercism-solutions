module WordCount  where

import Data.Char
import Data.List
import Data.Map (fromList)

wordCount s = fromList $ map (\s -> (head s, length s)) $ groupByWords s

normCase :: String -> String
normCase s = [ toLower x | x <- s ]

groupByWords :: String -> [ [ String ] ]
groupByWords s = groupBy (\x y -> x == y) $ sort $ words $ stripPunc $ normCase s

stripPunc :: String -> String
stripPunc s = [ x | x <- s, x `elem` allowedChars ]

allowedChars :: String
allowedChars = ['a'..'z'] ++ ['0'..'9'] ++ " "
