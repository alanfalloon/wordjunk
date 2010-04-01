
module ERandom where

import Control.Monad.State
import System.Random

type Entropy = Double

data RandomGen g => ERandom g = ER {
      entropy :: Entropy,
      gen :: g
    }

type ERandomM g a = State (ERandom g) a

runERandom :: RandomGen g => g -> ERandomM g a -> (a,g)
runERandom g m = (a,g')
    where
      (a, ER { gen=g' } ) = runState m (ER 0 g)

runERandomIO :: ERandomM StdGen a -> IO a
runERandomIO m = do
  g <- getStdGen
  let (a, g') = runERandom g m
  setStdGen g'
  return a

entropyM :: RandomGen g => ERandomM g Entropy
entropyM = gets entropy

-- | Calculate the entropy of a linear distribution of N choices
bitsLinear :: Integral a => a -> Entropy
bitsLinear n = logBase 2 (fromIntegral n)

-- | Get a random number in a range
eRandomRM :: (Integral a, Random a, RandomGen g) => (a,a) -> ERandomM g a
eRandomRM (lo,hi) = do
  ER e g <- get
  let (r,g') = randomR (lo,hi) g
      e'     = bitsLinear (abs (hi-lo))
  put (ER (e+e') g')
  return r

-- | Choose any element from the list with equal probibility
eRandomEltM :: RandomGen g => [a] -> ERandomM g a
eRandomEltM []         = fail "can't choose from empty list"
eRandomEltM (elt:elts) = eRandomEltM' 1 elt elts
    where
      eRandomEltM' _ e []        = return e
      eRandomEltM' n e (e':elts) = do
        x <- eRandomRM (1,n')
        if x == 1 then eRandomEltM' n' e' elts else eRandomEltM' n' e elts
          where
            n' :: Int
            n' = n + 1
