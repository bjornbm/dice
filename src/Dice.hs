module Dice where

import Control.Monad (replicateM, join)
import Data.List (intercalate, group, sort)
import Data.Ratio
import Data.Semigroup (mtimesDefault)
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



newtype Die = D { sides :: Int } deriving (Eq, Ord) -- ^ A single die.

data Hand = Hand { dice :: [Die], modifier :: Int }  -- ^ Several dice and/or a modifier to be summed.
          deriving (Eq, Ord)

-- | Specify a modifier.
die n = Hand [D n] 0
modif = Hand []

instance Semigroup Hand where
  Hand ds1 m1 <> Hand ds2 m2 = Hand (ds1 <> ds2) (m1 + m2)

instance Monoid Hand where mempty = modif 0


instance Show Die where
  show (D n) = "D" ++ show n
instance Show Hand where
  show (Hand ds m) = intercalate "+" (map showGroup $ group $ reverse $ sort ds)
                  <> showModifier m
    where
      showGroup xs = show1 (length xs) <> show (head xs) where
        show1 1 = ""
        show1 i = show i
      showModifier i = case compare i 0 of
        LT -> show i
        EQ -> ""
        GT -> "+" ++ show i

-- Convenience functions.
d :: Int -> Int -> Hand
m `d` n = mtimesDefault m (die n)
d `plus`  i = d <> modif i
d `minus` i = d <> modif (negate i)


d2   = D   2
d3   = D   3
d4   = D   4
d6   = D   6
d8   = D   8
d10  = D  10
d12  = D  12
d20  = D  20
d30  = D  30
d100 = D 100

-- | The possible sum of the .
outcomesD :: Die -> [Int]
outcomesD (D n) = [1..n]
outcomes1 :: Hand -> [Int]
outcomes1 (Hand ds m) = map ((+m) . sum) (mapM outcomesD ds)

outcomes :: [Hand] -> [[Int]]
outcomes = mapM outcomes1

outcomesN :: [Hand] -> Int
outcomesN = length . outcomes

rate :: ([Int] -> Bool) -> [Hand] -> Rate
rate p ds = length (filter p $ outcomes ds) % outcomesN ds
percent :: ([Int] -> Bool) -> [Hand] -> Percent
percent p = Percent . rate p
odds :: ([Int] -> Bool) -> [Hand] -> Odds
odds    p = Odds    . rate p


-- | Sample picks a random element from the list.
sample :: RandomGen g => [a] -> g -> (a, g)
sample xs gen = (xs !! i, g) where (i, g) = randomR (0, length xs - 1) gen

-- | Sample picks a random element from the list.
sampleIO :: [a] -> IO a
sampleIO xs = (xs !!) <$> randomRIO (0, length xs - 1)

roll :: Hand -> IO Int
roll = sampleIO . outcomes1

roll' :: Int -> (Int -> Int -> Hand) -> Int -> IO Int
roll' n d m = roll (n`d`m)
