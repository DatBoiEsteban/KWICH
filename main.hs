import Data.Char
import Data.HashSet as HashSet hiding (map, sort)
import Data.List
import System.IO
import Prelude

type NotSignificant = HashSet [Char]

main :: IO ()
main = do
  inpStr <- getLine
  let tokens = words inpStr
  let filename = head tokens
  ns <- openFile filename ReadMode
  hashNotSignificants <- getNotSignificantWords ns
  hClose ns
  ti <- openFile (tokens !! 1) ReadMode
  titlesList <- getTitles ti
  hClose ti
  let f = map (kwicTitles hashNotSignificants) titlesList
  let g = sortBy sortTitles f
  printaso g
  putStrLn (show g)

printaso :: [[String]] -> IO ()
printaso x = sequence_ (map printaso_2 x)

printaso_2 x = sequence_ (map putStrLn x)

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

titSigRotations :: NotSignificant -> [[Char]] -> [[[Char]]]
titSigRotations notSignificants xs = xs : [drop i xs ++ take i xs | i <- [0 .. n], not ((map toLower (xs !! i)) `elem` notSignificants)]
  where
    n = (length xs) - 1

putSpaces :: [[Char]] -> [Char]
putSpaces xss = tail (concat (map (' ' :) xss))

sep :: [[Char]] -> [[Char]]
sep xs = init xs ++ [last xs ++ " ><"]

kwicTitles :: NotSignificant -> [[Char]] -> [[Char]]
kwicTitles notS title =
  nub (map putSpaces (titSigRotations notS (sep title)))

sortTitles :: [[Char]] -> [[Char]] -> Ordering
sortTitles = (\(x : xs) (y : ys) -> compare (head x) (head y))

a (z : zs) = putStrLn (head z)
