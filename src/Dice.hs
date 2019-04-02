module Dice where

import Control.Monad (replicateM, join)
import Data.Ratio
import System.Random

type Rate = Ratio Int

newtype Percent = Percent Rate deriving (Ord, Eq)
instance Show Percent where
  show (Percent r) = show (round $ 100 * r) ++ " %"

newtype Odds = Odds Rate deriving (Ord, Eq)
instance Show Odds where
  show (Odds r) = show n ++ ":" ++ show (d - n)
    where
      n = numerator   r
      d = denominator r


data Dice = Int`D`Int
instance Show Dice where show (D n m) = show n ++ "D" ++ show m
d = D
d2   = (`D`  2)
d3   = (`D`  3)
d4   = (`D`  4)
d6   = (`D`  6)
d8   = (`D`  8)
d10  = (`D` 10)
d12  = (`D` 12)
d20  = (`D` 20)
d30  = (`D` 30)
d100 = (`D`100)

outcomes :: Dice -> [[Int]]
outcomes (n`D`m) = replicateM n [1..m]

outcomes' :: [Dice] -> [[Int]]
outcomes' = map join . mapM (outcomes)

outcomesN :: Dice -> Int
outcomesN (n`D`m) = m ^ n

rate :: ([Int] -> Bool) -> Dice -> Rate
rate p d = length (filter p $ outcomes d) % outcomesN d
percent :: ([Int] -> Bool) -> Dice -> Percent
percent p = Percent . rate p
odds :: ([Int] -> Bool) -> Dice -> Odds
odds    p = Odds    . rate p


-- | Sample picks a random element from the list.
sample :: RandomGen g => [a] -> g -> (a, g)
sample xs gen = (xs !! i, g) where (i, g) = randomR (0, length xs - 1) gen

-- | Sample picks a random element from the list.
sampleIO :: [a] -> IO a
sampleIO xs = (xs !!) <$> randomRIO (0, length xs - 1)

roll :: Dice -> IO [Int]
roll = sampleIO . outcomes

roll' :: Int -> (Int -> Int -> Dice) -> Int -> IO [Int]
roll' n d m = roll (n`d`m)
