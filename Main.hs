module Main where

import Data.Array.IArray
import ERandom
import System.Random
import Words

main :: IO ()
main = do
  ws <- loadWords 5
  junk <- runERandomIO (makeJunk 128 ws)
  putStrLn junk

makeJunk :: RandomGen g => Double -> [String] -> ERandomM g String
makeJunk bits words | bits <= 0 = return []
                    | otherwise = do
  w <- makeWord
  ws <- makeTrailingJunk bits
  return (w ++ ws)
    where
      makeWord = do
        w  <- eRandomEltM words
        w' <- randomizeWord w
        return w'

      makeTrailingJunk bits | bits <= 0 = return []
                            | otherwise = do
        j  <- eRandomEltM wordJoiners
        w  <- makeWord
        e  <- entropyM
        ws <- makeTrailingJunk (bits - e)
        return (j ++ w ++ ws)


randomizeWord :: RandomGen g => String -> ERandomM g String
randomizeWord []    = return []
randomizeWord (c:w) = do
  c' <- eRandomEltM alts
  w' <- randomizeWord w
  return (c':w')
    where
      alts = l33tLetters ! c
