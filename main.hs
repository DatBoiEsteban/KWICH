import Data.Char
import Data.Function
import Data.HashSet as HashSet hiding (map, sort)
import Data.List
import Data.List.Split
import Data.Ord
import System.Directory
import System.IO
import Prelude

type NotSignificant = HashSet [Char]

cargarHash :: FilePath -> IO NotSignificant
cargarHash fileName = do
  ns <- readFile fileName
  let notSignificant = getNotSignificantWords ns
  return notSignificant

cargarTitulos :: FilePath -> IO [[[Char]]]
cargarTitulos fileName = do
  ti <- readFile fileName
  let titlesList = getTitles ti
  return titlesList

crearArchivos :: [Char] -> IO ()
crearArchivos datos = do
  inpStr <- getLine
  let tokens = words inpStr
  hisf <- doesFileExist (tokens !! 0)
  if hisf
    then do
      putStrLn "JUEPUTA PUTA"
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
  crearArchivos (intercalate "\n" (alignOn (map laMayus enOrden)))

main2 :: IO ()
main2 = do
  inpStr <- getLine
  let tokens = words inpStr
  hashNotSignificants <- cargarHash (tokens !! 0)
  titlesList <- cargarTitulos (tokens !! 1)
  let titulosYRotaciones = concat (map (kwicTitles hashNotSignificants) titlesList)
  let enOrden = sortBy sortTitles titulosYRotaciones
  crearArchivos (intercalate "\n" enOrden)

printaso :: [String] -> IO ()
printaso x = sequence_ (map putStrLn x)

getNotSignificantWords :: [Char] -> NotSignificant
getNotSignificantWords inh = do
  let words = map (map toLower) (splitOn "\n" inh)
  HashSet.fromList words

getTitles :: [Char] -> [[[Char]]]
getTitles inh = do
  let titles = map (map toLower) (splitOn "\n" inh)
  map (splitOn " ") titles

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
titSigRotations notSignificants xs = [drop i xs ++ take i xs | i <- [0 .. n], not ((map toLower (xs !! i)) `elem` notSignificants)]
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

alignOn lines = map padline lines
  where
    partBeforechar = head . splitWhen isUpper
    longestLengthBeforeChar = maximum $ map (length . partBeforechar) lines
    padline line = replicate offset ' ' ++ line
      where
        offset = longestLengthBeforeChar - (length (partBeforechar line))

laMayus x = do
  let a = splitOn "><" x
  let b = splitOn " " (a !! 0)
  let c = map toUpper (b !! 0)
  let d = unwords . words
  d ((a !! 1) ++ " " ++ c ++ " " ++ (intercalate " " (tail b)))