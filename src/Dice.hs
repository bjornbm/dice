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
  Hand ds1 m1 <> Hand ds2 m2 = Hand (ds1 <> ds2) (m1 + m2)
  Hand ds  m1 <> Modifier m2 = Hand ds (m1 + m2)
  Hand ds  m  <> D n = Hand (D n:ds) m

  Modifier m1 <> Hand ds  m2 = Hand ds  (m1 + m2)
  Modifier m1 <> Modifier m2 = Modifier (m1 + m2)
  Modifier m  <> D n = Hand [D n] m

  D n  <> Hand ds  m = Hand (D n:ds)     m
  D n  <> Modifier m = Hand [D n]        m
  D n1 <> D n2       = Hand [D n1, D n2] 0

instance Monoid Dice where mempty = Modifier 0

data Dice = D { sides :: Int }
          | Hand { dice :: [Dice], modifier :: Int }
          | Modifier { modifier :: Int }
          deriving (Eq, Ord)

instance Show Dice where
  show (D n) = "D" ++ show n
  show (Hand ds m) = intercalate "+" (map showGroup $ group $ reverse $ sort ds)
                       <> show (Modifier m)
    where
      showGroup xs = show1 (length xs) <> show (head xs) where
        show1 1 = ""
        show1 i = show i
  show (Modifier i) = case compare i 0 of
    LT -> show i
    EQ -> ""
    GT -> "+" ++ show i


m `d` n = mtimesDefault m (D n)
d `plus`  i = d <> Modifier i
d `minus` i = d <> Modifier (negate i)

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
outcomes1 :: Dice -> [Int]
outcomes1 (D n) = [1..n]
outcomes1 (Modifier i) = [i]
outcomes1 (Hand ds m) = map ((+m) . sum) (mapM outcomes1 ds)

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
