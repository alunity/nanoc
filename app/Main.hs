module Main where

import Lexer (tokenise)
import System.IO hiding (readFile)
import Prelude hiding (readFile)
import Parser (parseExpression)

main :: IO ()
main = do
  content <- readFile "./local/main.c"
  let tokens = tokenise content
  case tokens of
    -- Just ts -> print ts
    Just ts -> print (parseExpression ts)
    Nothing -> print "Tokenisation failed"

readFile :: FilePath -> IO String
readFile p = do
  handle <- openFile p ReadMode
  hGetContents handle
