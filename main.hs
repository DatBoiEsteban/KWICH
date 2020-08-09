import System.IO

main :: IO ()
main = do
  inpStr <- getLine
  let tokens = words inpStr
  let filename = head tokens
  inh <- openFile filename ReadMode
  f filename

f a = putStrLn a