{- |
Module      :  Main
Description :  This program creates a KWIC index with the significant words of book titles.
Copyright   :  Esteban Mata, Esteban Salas and Walter López. 
License     :  Tecnológico de Costa Rica.

Maintainer  :  esteban1113001@gmail.com
Stability   :  stable | experimental | research
Portability :  portable 
-}

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


-- |La función cargarHash rellena el HashSet de palabras no significativa desde un archivo de texto.
cargarHash :: FilePath -> IO NotSignificant
cargarHash fileName = do
  ns <- readFile fileName
  let notSignificant = getNotSignificantWords ns
  return notSignificant


-- |La función cargarTitulos rellena una lista con títulos desde un archivo de texto.
cargarTitulos :: FilePath -> IO [[[Char]]]
cargarTitulos fileName = do
  ti <- readFile fileName
  let titlesList = getTitles ti
  return titlesList


-- |La función crearArchivos crear el archivo.txt de salida para imprimir el indice.
crearArchivos :: [Char] -> IO ()
crearArchivos datos = do
  inpStr <- getLine
  let tokens = words inpStr
  hisf <- doesFileExist (tokens !! 0)
  if hisf
    then do
      putStrLn "El nombre del archivo ya existe, intente otro nombre."
      crearArchivos datos
    else do
      writeFile (tokens !! 0) datos


-- |La función kwicAlineado genera el indice kwic con la salida alineada en cada palabra que inicia las rotaciones (y las coloca en mayúcula).
kwicAlineado :: IO ()
kwicAlineado = do
  inpStr <- getLine
  let tokens = words inpStr
  hashNotSignificants <- cargarHash (tokens !! 0)
  titlesList <- cargarTitulos (tokens !! 1)
  let titulosYRotaciones = concat (map (kwicTitles hashNotSignificants) titlesList)
  let enOrden = sortBy sortTitles titulosYRotaciones
  crearArchivos (intercalate "\n" (alignOn (map laMayus enOrden)))


-- |La función kwicStandard genera el indice kwic standard con los separadores <>.
kwicStandard :: IO ()
kwicStandard = do
  inpStr <- getLine
  let tokens = words inpStr
  hashNotSignificants <- cargarHash (tokens !! 0)
  titlesList <- cargarTitulos (tokens !! 1)
  let titulosYRotaciones = concat (map (kwicTitles hashNotSignificants) titlesList)
  let enOrden = sortBy sortTitles titulosYRotaciones
  crearArchivos (intercalate "\n" enOrden)


-- |La función print imprime en la pantalla.
print :: [String] -> IO ()
print x = sequence_ (map putStrLn x)


-- |La función getNotSignificantWords obtiene las palabras no significativas, las convierte a minúscula y las inserta en un HashSet.
getNotSignificantWords :: [Char] -> NotSignificant
getNotSignificantWords inh = do
  let words = map (map toLower) (splitOn "\n" inh)
  HashSet.fromList words


-- |La función getTitles obtiene los títulos, los convierte a minúsculas y los inserta en una lista.
getTitles :: [Char] -> [[[Char]]]
getTitles inh = do
  let titles = map (map toLower) (splitOn "\n" inh)
  map (splitOn " ") titles


-- |La función cargarHash rellena el HashSet de palabras no significativa desde el archivo.
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


-- |La función cargarHash rellena el HashSet de palabras no significativa desde el archivo.
toWords :: [Char] -> [[Char]]
toWords [] = []
toWords (x : xs)
  | x == ' ' = toWords (dropWhile (' ' ==) xs)
  | otherwise = (x : takeWhile (' ' /=) xs) : toWords (dropWhile (' ' /=) xs)


-- |La función cargarHash rellena el HashSet de palabras no significativa desde el archivo.
titSigRotations :: NotSignificant -> [[Char]] -> [[[Char]]]
titSigRotations notSignificants xs = [drop i xs ++ take i xs | i <- [0 .. n], not ((map toLower (xs !! i)) `elem` notSignificants)]
  where
    n = (length xs) - 1

    
-- |La función cargarHash rellena el HashSet de palabras no significativa desde el archivo.
putSpaces :: [[Char]] -> [Char]
putSpaces xss = tail (concat (map (' ' :) xss))


-- |La función cargarHash rellena el HashSet de palabras no significativa desde el archivo.
sep :: [[Char]] -> [[Char]]
sep xs = init xs ++ [last xs ++ " ><"]


-- |La función cargarHash rellena el HashSet de palabras no significativa desde el archivo.
kwicTitles :: NotSignificant -> [[Char]] -> [[Char]]
kwicTitles notS title =
  nub (map putSpaces (titSigRotations notS (sep title)))


-- |La función cargarHash rellena el HashSet de palabras no significativa desde el archivo.
sortTitles :: [Char] -> [Char] -> Ordering
sortTitles = (\(x : xs) (y : ys) -> compare x y)


-- |La función cargarHash rellena el HashSet de palabras no significativa desde el archivo.
alignOn lines = map padline lines
  where
    partBeforechar = head . splitWhen isUpper
    longestLengthBeforeChar = maximum $ map (length . partBeforechar) lines
    padline line = replicate offset ' ' ++ line
      where
        offset = longestLengthBeforeChar - (length (partBeforechar line))
   

-- |La función cargarHash rellena el HashSet de palabras no significativa desde el archivo.
laMayus x = do
  let a = splitOn "><" x
  let b = splitOn " " (a !! 0)
  let c = map toUpper (b !! 0)
  let d = unwords . words
  d ((a !! 1) ++ " " ++ c ++ " " ++ (intercalate " " (tail b)))