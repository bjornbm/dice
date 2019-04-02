module Dice where

import Control.Monad (replicateM, join)
import Data.List (intercalate)
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

instance Semigroup Dice where
  d1 <> d2 = Different [d1, d2]

data Dice = One { sides :: Int }
          | Many { count :: Int, sides :: Int }
          | Different { dice :: [Dice] }
          | Modifier { modifier :: Int }
instance Show Dice where
  show (One n) = "D" ++ show n
  show (Many m n) = show m ++ "D" ++ show n
  show (Different ds) = intercalate "+" $ map show ds  -- TODO not good for modifiers
  show (Modifier i) = show i

d `plus`  i = d <> Modifier i
d `minus` i = d <> Modifier (negate i)

d = Many
d2   = One   2
d3   = One   3
d4   = One   4
d6   = One   6
d8   = One   8
d10  = One  10
d12  = One  12
d20  = One  20
d30  = One  30
d100 = One 100

-- | The possible sum of the .
outcomes1 :: Dice -> [Int]
outcomes1 (One n) = [1..n]
outcomes1 (Many m n) = map sum . replicateM m $ outcomes1 (One n)
outcomes1 (Modifier i) = [i]
outcomes1 (Different ds) = map sum $ mapM outcomes1 ds

outcomes :: [Dice] -> [[Int]]
outcomes = mapM outcomes1

outcomesN :: [Dice] -> Int
outcomesN = length . outcomes

rate :: ([Int] -> Bool) -> [Dice] -> Rate
rate p ds = length (filter p $ outcomes ds) % outcomesN ds
percent :: ([Int] -> Bool) -> [Dice] -> Percent
percent p = Percent . rate p
odds :: ([Int] -> Bool) -> [Dice] -> Odds
odds    p = Odds    . rate p


-- | Sample picks a random element from the list.
sample :: RandomGen g => [a] -> g -> (a, g)
sample xs gen = (xs !! i, g) where (i, g) = randomR (0, length xs - 1) gen

-- | Sample picks a random element from the list.
sampleIO :: [a] -> IO a
sampleIO xs = (xs !!) <$> randomRIO (0, length xs - 1)

roll :: Dice -> IO Int
roll = sampleIO . outcomes1

roll' :: Int -> (Int -> Int -> Dice) -> Int -> IO Int
roll' n d m = roll (n`d`m)
