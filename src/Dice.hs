{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dice where

import Data.List (intercalate, group, sortOn)
import Data.Ord (Down (Down))
import Data.Ratio
import Data.Semigroup (mtimesDefault)
import System.Random

-- Generic rate representations
-- ============================

type Rate = Rational -- Ratio Integer

newtype Percent = Percent Rate deriving (Ord, Eq, Num)
instance Show Percent where
  show (Percent r) = show (round $ 100 * r :: Integer) ++ " %"

newtype Odds = Odds Rate deriving (Ord, Eq)
instance Show Odds where
  show (Odds r) = show num ++ ":" ++ show (den - num)
    where
      num = numerator   r
      den = denominator r


-- Generic sampling
-- ================

-- | Sample picks a random element from the list.
sample :: RandomGen g => [a] -> g -> (a, g)
sample xs gen = (xs !! i, g) where (i, g) = randomR (0, length xs - 1) gen

-- | Sample picks a random element from the list.
sampleIO :: [a] -> IO a
sampleIO xs = (xs !!) <$> randomRIO (0, length xs - 1)


-- Types for dice
-- ==============

type Sides = Int  -- ^ The number of sides on a die.
type Roll  = Int  -- ^ The outcome of rolling a hand.
type Count = Int  -- ^ The number of dice to roll.
type Mod   = Int  -- ^ A modifier to apply to the roll.

-- | A hand is a set of dice and any modifier that goes with them.
  -- When rolled they are all added together.
data Hand = Hand { dice :: [Sides], modifier :: Mod } deriving (Eq, Ord)

instance Semigroup Hand where
  Hand ds1 m1 <> Hand ds2 m2 = Hand (ds1 <> ds2) (m1 + m2)

instance Monoid Hand where mempty = Hand [] 0

instance Show Hand where
  show (Hand ds m) = showDice ds <> showModifier m
    where
      showDice = intercalate "+" . map showGroup . group . reverseSort
      showGroup xs = show1 (length xs) <> showD (head xs) where
        show1 1 = ""
        show1 i = show i
        showD s = "D" ++ show s
      showModifier i = case compare i 0 of
        LT -> show i
        EQ -> ""
        GT -> "+" ++ show i
      reverseSort = sortOn Down


-- Convenience functions
-- =====================

-- | Specify a single die.
die :: Sides -> Hand
die s = Hand [s] 0

-- | Specify a modifier.
modif :: Mod -> Hand
modif = Hand []

-- | @m`d`n@ means m n-sided dice.
d :: Count -> Sides -> Hand
n `d` s = mtimesDefault n (die s)

-- | Add/subtract a modifier from a 'Hand'.
plus, minus :: Hand -> Mod -> Hand
h `plus`  m = h <> modif m
h `minus` m = h <> modif (negate m)

-- | Various dice.
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


-- Analysing outcomes
-- ==================

-- | Generate one of the possible outcomes (sums) of rolling a given
  -- hand.
outcome1 :: Hand -> IO Roll
outcome1 (Hand ds m) = do
  faces <- mapM (\s -> sampleIO [1..s]) ds
  return $ sum faces + m

-- | Generate one of the possible outcomes (sums) of rolling several
  -- hand.
outcome :: [Hand] -> IO [Roll]
outcome = mapM outcome1

-- | Enumerate the outcomes (sums) of each possible combination of
  -- dice faces for a given hand. The outcomes will not be sorted.
outcomes1 :: Hand -> [Roll]
outcomes1 (Hand ds m) = (+m) . sum <$> mapM (\s -> [1..s]) ds

-- | Enumerate the outcomes (sums) of each possible combination of
  -- dice faces for several hands. The outcomes will not be sorted.
outcomes :: [Hand] -> [[Roll]]
outcomes = mapM outcomes1

-- | Provide the rate with which the hands can be expected to fullfil the
  -- given predicate.
rate :: ([Roll] -> Bool) -> [Hand] -> Rate
rate p h = let os = outcomes h in length' (filter p os) % length' os
  where length' = fromIntegral . length

-- | Provide the percentage of rolls that the hands can be expected to
  -- fullfil the given predicate.
percent :: ([Roll] -> Bool) -> [Hand] -> Percent
percent p = Percent . rate p

-- | Provide the odds of the hands fullfilling the given predicate.
odds :: ([Roll] -> Bool) -> [Hand] -> Odds
odds p = Odds . rate p


-- Random rolls
-- ============

-- | Perform a random roll of the hand.
roll1 :: Hand -> IO Roll
roll1 = outcome1 -- sampleIO . outcomes1

-- | Perform a random roll of the hands.
roll :: [Hand] -> IO [Roll]
roll = outcome -- sampleIO . outcomes

-- | @roll' 2 d 6@ is the same as @roll (2`d`6)@.
roll' :: Count -> (Count -> Sides -> Hand) -> Sides -> IO Roll
roll' n _ s = roll1 (n`d`s)
