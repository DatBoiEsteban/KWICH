import Data.Char
import Data.HashSet as HashSet hiding (map, sort)
import Data.List (map, sort)
import System.IO
import Prelude

type NotSignificant = HashSet [Char]

getNotSignificantWords :: Handle -> IO NotSignificant
getNotSignificantWords inh =
  do
    words <- getNotSignificantWordsAux inh []
    let hashpals = HashSet.fromList words
    return hashpals

getNotSignificantWordsAux :: Handle -> [[Char]] -> IO [[Char]]
getNotSignificantWordsAux inh words =
  do
    ineof <- hIsEOF inh
    if ineof
      then do
        return words
      else do
        inpStr <- hGetLine inh
        let minus = map toLower inpStr
        getNotSignificantWordsAux inh (words ++ [minus])

main :: IO ()
main = do
  inpStr <- getLine
  let tokens = words inpStr
  let filename = head tokens
  inh <- openFile filename ReadMode
  hashNotSignificants <- getNotSignificantWords inh
  hClose inh
  putStrLn (show hashNotSignificants)