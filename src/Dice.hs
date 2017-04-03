module Dice where

import Control.Monad (replicateM)
import Data.Ratio

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

outcomes :: Dice -> [[Int]]
outcomes (n`D`m) = replicateM n [1..m]

outcomesN :: Dice -> Int
outcomesN (n`D`m) = m ^ n

rate :: ([Int] -> Bool) -> Dice -> Rate
rate p d = length (filter p $ outcomes d) % outcomesN d
percent :: ([Int] -> Bool) -> Dice -> Percent
percent p = Percent . rate p
odds :: ([Int] -> Bool) -> Dice -> Odds
odds    p = Odds    . rate p
