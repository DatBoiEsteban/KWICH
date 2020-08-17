{- |
Module      :  Main
Description :  This program creates a KWIC index with the significant words of book titles.
Copyright   :  Esteban Mata, Esteban Salas and Walter López. 
License     :  Tecnológico de Costa Rica.

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


-- |Rellena el HashSet de palabras no significativas desde un archivo de texto.
cargarHash :: FilePath -> IO NotSignificant
cargarHash fileName = do
  ns <- readFile fileName
  let notSignificant = getNotSignificantWords ns
  return notSignificant


-- |Rellena una lista con títulos desde un archivo de texto.
cargarTitulos :: FilePath -> IO [[[Char]]]
cargarTitulos fileName = do
  ti <- readFile fileName
  let titlesList = getTitles ti
  return titlesList


-- |Crea el archivo.txt de salida para imprimir el índice.
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


-- |Genera el índice kwic con la salida alineada en cada palabra que inicia las rotaciones (y las coloca en mayúscula).
kwicAlineado :: IO ()
kwicAlineado = do
  inpStr <- getLine
  let tokens = words inpStr
  hashNotSignificants <- cargarHash (tokens !! 0)
  titlesList <- cargarTitulos (tokens !! 1)
  let titulosYRotaciones = concat (map (kwicTitles hashNotSignificants) titlesList)
  let enOrden = sortBy sortTitles titulosYRotaciones
  crearArchivos (intercalate "\n" (alignOn (map laMayus enOrden)))


-- |Genera el índice kwic standard (con los separadores <>).
kwicStandard :: IO ()
kwicStandard = do
  inpStr <- getLine
  let tokens = words inpStr
  hashNotSignificants <- cargarHash (tokens !! 0)
  titlesList <- cargarTitulos (tokens !! 1)
  let titulosYRotaciones = concat (map (kwicTitles hashNotSignificants) titlesList)
  let enOrden = sortBy sortTitles titulosYRotaciones
  crearArchivos (intercalate "\n" enOrden)


-- |Imprime en la pantalla.
print :: [String] -> IO ()
print x = sequence_ (map putStrLn x)


-- |Obtiene las palabras no significativas, las convierte en minúscula y las inserta en un HashSet.
getNotSignificantWords :: [Char] -> NotSignificant
getNotSignificantWords inh = do
  let words = map (map toLower) (splitOn "\n" inh)
  HashSet.fromList words


-- |Obtiene los títulos, los convierte en minúsculas y los inserta en una lista.
getTitles :: [Char] -> [[[Char]]]
getTitles inh = do
  let titles = map (map toLower) (splitOn "\n" inh)
  map (splitOn " ") titles


-- |Realiza las rotaciones de los títulos en las palabras significativas.
titSigRotations :: NotSignificant -> [[Char]] -> [[[Char]]]
titSigRotations notSignificants xs = [drop i xs ++ take i xs | i <- [0 .. n], not ( (xs !! i) `elem` notSignificants)]
  where
    n = (length xs) - 1

    
-- |Coloca espacios (' ').
putSpaces :: [[Char]] -> [Char]
putSpaces xss = tail (concat (map (' ' :) xss))


-- |Coloca ¨><¨ que se utiliza como separadores en las rotaciones.
sep :: [[Char]] -> [[Char]]
sep xs = init xs ++ [last xs ++ " ><"]


-- |Genera el indice KWIC con sus respectivas rotaciones y formato.
kwicTitles :: NotSignificant -> [[Char]] -> [[Char]]
kwicTitles notS title =
  nub (map putSpaces (titSigRotations notS (sep title)))


-- |Ordena los títulos alfabéticamente.
sortTitles :: [Char] -> [Char] -> Ordering
sortTitles = (\(x) (y) -> compare x y)


-- |Alínea las rotaciones en las letras mayúsculas.
alignOn lines = map padline lines
  where
    partBeforechar = head . splitWhen isUpper
    longestLengthBeforeChar = maximum $ map (length . partBeforechar) lines
    padline line = replicate offset ' ' ++ line
      where
        offset = longestLengthBeforeChar - (length (partBeforechar line))
   

-- |Convierte a mayúsculas la palabra significativa que inicia la rotación.
laMayus x = do
  let a = splitOn "><" x
  let b = splitOn " " (a !! 0)
  let c = map toUpper (b !! 0)
  let d = unwords . words
  d ((a !! 1) ++ " " ++ c ++ " " ++ (intercalate " " (tail b)))