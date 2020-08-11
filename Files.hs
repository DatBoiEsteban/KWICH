module Files
  ( getNotSignificantWords,
    getTitles,
    NotSignificant,
  )
where

import Data.Char
import Data.HashSet as HashSet hiding (map, sort)
import Data.List (map, sort)
import System.IO
import Prelude

type NotSignificant = HashSet [Char]

getNotSignificantWords :: Handle -> IO NotSignificant
getNotSignificantWords inh = do
  words <- getNotSignificantWordsAux inh []
  let hashpals = HashSet.fromList words
  return hashpals

getNotSignificantWordsAux :: Handle -> [[Char]] -> IO [[Char]]
getNotSignificantWordsAux inh words = do
  ineof <- hIsEOF inh
  if ineof
    then do
      return words
    else do
      inpStr <- hGetLine inh
      let minus = map toLower inpStr
      getNotSignificantWordsAux inh (words ++ [minus])

getTitles :: Handle -> IO [[[Char]]]
getTitles inh = getTitlesAux inh []

getTitlesAux :: Handle -> [[[Char]]] -> IO [[[Char]]]
getTitlesAux inh titles = do
  ineof <- hIsEOF inh
  if ineof
    then do
      return titles
    else do
      curTitle <- hGetLine inh
      let minus = map toLower curTitle
      let title = toWords minus
      getTitlesAux inh (titles ++ [title])

toWords :: [Char] -> [[Char]]
toWords [] = []
toWords (x : xs)
  | x == ' ' = toWords (dropWhile (' ' ==) xs)
  | otherwise = (x : takeWhile (' ' /=) xs) : toWords (dropWhile (' ' /=) xs)