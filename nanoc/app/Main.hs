module Main where

import Lexer (tokenise)
import System.IO hiding (readFile)
import Prelude hiding (readFile)

main :: IO ()
main = do
  content <- readFile "main.c"
  let tokens = tokenise content
  case tokens of
    Just ts -> print ts
    Nothing -> print "Tokenisation failed"

readFile :: FilePath -> IO String
readFile p = do
  handle <- openFile p ReadMode
  hGetContents handle
