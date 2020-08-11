import Files
import Rotations
import System.IO

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
  putStrLn (show (titSigRotations (titlesList !! 23) hashNotSignificants))