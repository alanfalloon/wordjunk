module Main where

import Data.Array.IArray
import ERandom hiding (entropy)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.Random
import Words

header = "Usage: wordjunk [options]"

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['V'] ["version"] (NoArg showVersion) "show the current version and exit."
    , Option ['h'] ["help"]    (NoArg showHelp)    "Show this help and exit."
    , Option ['e'] ["entropy"] (ReqArg setEntropy "BITS") entropyHelp
    , Option ['n'] ["count"]   (ReqArg setCount "N") "How many words to generate."
    ]
    where
      entropyHelp = "The minimum amount of entropy in the generated" ++
                    " word. Entropy is measured in bits, more bits means" ++
                    " a longer word, but stronger password. The default" ++
                    " value is 128 bits, which is enough for almost" ++
                    " anyone"

data Options = Options {
      count :: Int,
      entropy :: Entropy
    }

main :: IO ()
main = do
  args <- getArgs
  flags <- case getOpt RequireOrder options args of
            (o, [], []) -> return o
            (_, fl, []) -> error $ "unrecognized arguments: " ++ unwords fl
            (_, _ , m ) -> error $ concat m ++ usageInfo header options
  opts <- foldl (>>=) (return defaultOptions) flags
  ws <- loadWords
  sequence_ $ replicate (count opts) $ do
         junk <- runERandomIO (makeJunk (entropy opts) ws)
         putStrLn junk

makeJunk :: (Random i, Ix i, RandomGen g) =>
            Double -> Array i String -> ERandomM g String
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

showVersion :: Options -> IO Options
showVersion _ = do
  putStrLn ver
  exitWith ExitSuccess
      where
        ver = unlines
                [ "wordjunk 0.1"
                , "Copyright (C) 2010 Alan Falloon <alan.falloon@gmail.com>"
                , "This program is free software; " ++
                  "you may redistribute it under the terms of"
                , "the GNU General Public License version 3 or" ++
                  " (at your option) a later version."
                , "This program has absolutely no warranty."
                ]

showHelp :: Options -> IO Options
showHelp _ = do
  putStrLn $ usageInfo header options
  exitWith ExitSuccess

setEntropy :: String -> Options -> IO Options
setEntropy entStr opt = do
  ent <- convertArg "entropy" "number" entStr
  return (opt { entropy = ent })

setCount :: String -> Options -> IO Options
setCount countStr opt = do
  c <- convertArg "count" "integer" countStr
  return (opt { count = c })

convertArg :: Read a => String -> String -> String -> IO a
convertArg name vtype val = do
  case reads val of
    [(x,"")] -> return x
    _        -> error $ name ++ " must be a " ++ vtype ++ ", not \"" ++ val ++ "\""

defaultOptions = Options {
                   count = 1,
                   entropy = 128
                 }
