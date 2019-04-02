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

instance Semigroup Dice where
  Different ds1 m1 <> Different ds2 m2 = Different (ds1 <> ds2) (m1 + m2)
  Different ds  m1 <> Modifier m2 = Different ds (m1 + m2)
  Different ds  m  <> One n = Different (One n:ds) m

  Modifier m1 <> Different ds m2 = Different ds (m1 + m2)
  Modifier m1 <> Modifier m2 = Modifier (m1 + m2)
  Modifier m  <> One n = Different [One n] m

  One n <> Different ds m = Different (One n:ds) m
  One n <> Modifier m     = Different [One n]    m
  One n1 <> One n2 = Different [One n1, One n2] 0

instance Monoid Dice where mempty = Modifier 0

data Dice = One { sides :: Int }
          | Different { dice :: [Dice], modifier :: Int }
          | Modifier { modifier :: Int }
          deriving (Eq, Ord)

instance Show Dice where
  show (One n) = "D" ++ show n
  show (Different ds m) = intercalate "+" (map showGroup $ group $ reverse $ sort ds)
                       <> show (Modifier m)
    where
      showGroup xs = show1 (length xs) <> show (head xs) where
        show1 1 = ""
        show1 i = show i
  show (Modifier i) = case compare i 0 of
    LT -> show i
    EQ -> ""
    GT -> "+" ++ show i


m `d` n = mtimesDefault m (One n)
d `plus`  i = d <> Modifier i
d `minus` i = d <> Modifier (negate i)

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
outcomes1 (Modifier i) = [i]
outcomes1 (Different ds m) = map ((+m) . sum) (mapM outcomes1 ds)

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
