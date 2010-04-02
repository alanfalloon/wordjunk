module Words (loadWords,
              wordJoiners,
              l33tLetters) where

import Control.Monad
import Data.Array.IArray
import Data.Char
import Data.List
import System.IO

-- | Load the words from the system dictionary, and only keep the ones
-- | within the specified word range, and which consist of only
-- | lowercase letters.
loadWords :: IO (Array Int String)
loadWords = do
  contents <- readFile "/usr/share/dict/words"
  let allWords = words contents
      goodWords = filter goodWord allWords
      gwArray = listToArray goodWords
  return gwArray
      where
        lenRange = (3,10)
        goodWord w = shortEnough && noCaps
            where
              shortEnough = inRange lenRange (length w)
              noCaps      = all isLower w
              

wordJoiners :: Array Int String
wordJoiners = listToArray [ "1" ,
                            "2" ,
                            "4" ,
                            "8" ,
                            "-" ,
                            "_" ,
                            " " ,
                            "!" ,
                            "?" ,
                            "+" ,
                            "~" ,
                            "^" ,
                            "&" ,
                            "=" ,
                            "#" ,
                            "*" ,
                            "|" ,
                            "$" ,
                            "@" ,
                            "%" ,
                            "." ]

l33tLetters :: Array Char (Array Int Char)
l33tLetters = listArray (minBound, maxBound) [ choicesForChar c | c <- allChars ]
    where
      choicesForChar c | isUpper c = l33tLetters ! toLower c
                       | isLower c = listToArray $ c : toUpper c : alts ! c
                       | otherwise = singleton c

      alts :: Array Char String
      alts = accumArray (++) [] ('a', 'z') [ ('o', "0"   ) ,
                                             ('l', "7") ,
                                             ('i', "1!|" ) ,
                                             ('z', "2"   ) ,
                                             ('e', "3"   ) ,
                                             ('a', "4@"  ) ,
                                             ('s', "5$"  ) ,
                                             ('x', "%"   ) ]

      singleton c = array (1,1) [(1,c)]
      allChars    = enumFromTo minBound maxBound

listToArray :: [a] -> Array Int a
listToArray x = listArray (1,length x) x
