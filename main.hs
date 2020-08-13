import Data.Char
import Data.HashSet as HashSet hiding (map, sort)
import Data.List
import Data.List.Split
import System.Directory
import System.IO
import Prelude

type NotSignificant = HashSet [Char]

cargarHash fileName = do
  ns <- readFile fileName
  let hashNotSignificants = getNotSignificantWords ns
  hClose ns
  hashNotSignificants

cargarTitulos :: FilePath -> IO [[[Char]]]
cargarTitulos fileName = do
  ti <- openFile fileName ReadMode
  let titlesList = getTitles ti
  hClose ti
  titlesList

crearArchivos :: [Char] -> IO ()
crearArchivos datos = do
  inpStr <- getLine
  let tokens = words inpStr
  hisf <- doesFileExist (tokens !! 0)
  if hisf
    then do
      crearArchivos datos
    else do
      writeFile (tokens !! 0) datos

main :: IO ()
main = do
  inpStr <- getLine
  let tokens = words inpStr
  hashNotSignificants <- cargarHash (tokens !! 0)
  titlesList <- cargarTitulos (tokens !! 1)
  let titulosYRotaciones = concat (map (kwicTitles hashNotSignificants) titlesList)
  let enOrden = sortBy sortTitles titulosYRotaciones
  printaso enOrden
  crearArchivos (show enOrden)

printaso :: [String] -> IO ()
printaso x = sequence_ (map putStrLn x)

getNotSignificantWords inh = do
  words <- getNotSignificantWordsAux inh []
  let hashpals = HashSet.fromList words
  return hashpals

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

sortTitles :: [Char] -> [Char] -> Ordering
sortTitles = (\(x : xs) (y : ys) -> compare x y)
