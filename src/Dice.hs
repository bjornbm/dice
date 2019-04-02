module Dice where

import Data.List (intercalate, group, sortOn)
import Data.Ord (Down (Down))
import Data.Ratio
import Data.Semigroup (mtimesDefault)
import System.Random

type Rate = Ratio Int

newtype Percent = Percent Rate deriving (Ord, Eq)
instance Show Percent where
  show (Percent r) = show (round $ 100 * r :: Int) ++ " %"

newtype Odds = Odds Rate deriving (Ord, Eq)
instance Show Odds where
  show (Odds r) = show num ++ ":" ++ show (den - num)
    where
      num = numerator   r
      den = denominator r


data Hand = Hand { dice :: [Int], modifier :: Int }  -- ^ Several dice and/or a modifier to be summed.
          deriving (Eq, Ord)

-- | Specify a modifier.
die, modif :: Int -> Hand
die n = Hand [n] 0
modif = Hand []

instance Semigroup Hand where
  Hand ds1 m1 <> Hand ds2 m2 = Hand (ds1 <> ds2) (m1 + m2)

instance Monoid Hand where mempty = modif 0


instance Show Hand where
  show (Hand ds m) = showDice ds <> showModifier m
    where
      showDice = intercalate "+" . map showGroup . group . reverseSort
      showGroup xs = show1 (length xs) <> showD (head xs) where
        show1 1 = ""
        show1 i = show i
        showD n = "D" ++ show n
      showModifier i = case compare i 0 of
        LT -> show i
        EQ -> ""
        GT -> "+" ++ show i
      reverseSort = sortOn Down

-- Convenience functions.
d :: Int -> Int -> Hand
m `d` n = mtimesDefault m (die n)
plus, minus :: Hand -> Int -> Hand
h `plus`  i = h <> modif i
h `minus` i = h <> modif (negate i)

d2, d3, d4, d6, d8, d10, d12, d20, d30, d100 :: Hand
d2   = die   2
d3   = die   3
d4   = die   4
d6   = die   6
d8   = die   8
d10  = die  10
d12  = die  12
d20  = die  20
d30  = die  30
d100 = die 100


outcomes1 :: Hand -> [Int]
outcomes1 (Hand ds m) = map ((+m) . sum) (mapM (\n -> [1..n]) ds)

outcomes :: [Hand] -> [[Int]]
outcomes = mapM outcomes1

outcomesN :: [Hand] -> Int
outcomesN = length . outcomes

rate :: ([Int] -> Bool) -> [Hand] -> Rate
rate p h = let os = outcomes h in length (filter p os) % length os
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

roll1 :: Hand -> IO Int
roll1 = sampleIO . outcomes1

roll :: [Hand] -> IO [Int]
roll = sampleIO . outcomes

roll' :: Int -> (Int -> Int -> Hand) -> Int -> IO Int
roll' n _ m = roll1 (n`d`m)
