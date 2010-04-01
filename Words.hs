module Words where

import Control.Monad
import Data.Array.IArray
import Data.Char
import Data.List
import System.IO

loadWords :: Int -> IO [String]
loadWords maxLen = do
  contents <- readFile "/usr/share/dict/words"
  let allWords = words contents
      goodWords = filter goodWord allWords
  return goodWords
      where
        goodWord w = shortEnough && noCaps
            where
              shortEnough = length w < maxLen
              noCaps      = all isLower w
              

wordJoiners :: [String]
wordJoiners = [ "1" ,
                "2" ,
                "4" ,
                "8" ,
                "-" ,
                "_" ,
                "!" ,
                "#" ,
                "*" ,
                "|" ,
                "$" ,
                "@" ,
                "%" ,
                "." ]

l33tLetters :: Array Char String
l33tLetters = accumArray (++) [] (minBound, maxBound) letters
    where
      letters    = itself ++ caseChange ++ alternates
      itself     = [ (c,[c]) | c <- enumFromTo minBound maxBound ]
      caseChange = [ x | low <- enumFromTo 'a' 'z',
                         x <- [ (low, [toUpper low]), (toUpper low, [low]) ] ]
      alternates = dupUpper [ ('o' ,"0"   ) ,
                              ('l' ,"17!|") ,
                              ('i' ,"1!|" ) ,
                              ('z' ,"2"   ) ,
                              ('e' ,"3"   ) ,
                              ('a' ,"4@"  ) ,
                              ('s' ,"5$"  ) ,
                              ('x' ,"%"   ) ]
      dupUpper elts = do
        elt@(c,x) <- elts
        return elt `mplus` return (toUpper c, x)
