module Rotations
  ( titSigRotations,
  )
where

import Data.Char
import Files

titSigRotations :: [[Char]] -> NotSignificant -> [[[Char]]]
titSigRotations xs notSignificants = xs : [drop i xs ++ take i xs | i <- [0 .. n], not ((map toLower (xs !! i)) `elem` notSignificants)]
  where
    n = (length xs) - 1