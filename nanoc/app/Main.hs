module Main where
import System.IO hiding (readFile)
import Prelude hiding (readFile)

main :: IO ()
main = do
          content <- readFile "x.txt"
          putStrLn content 

readFile :: FilePath -> IO String
readFile p = do 
                handle <- openFile p ReadMode
                hGetContents handle 


