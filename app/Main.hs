module Main where

import Lexer (Token (Comment), tokenise)
import Parser (Expression, parseExpression)
import System.IO hiding (readFile)
import Prelude hiding (readFile)
import Debug.Trace (trace)

main :: IO ()
main = do
  content <- readFile "./local/main.c"
  case compile content of
    Nothing -> putStrLn "Compile/parse failed"
    Just expr -> print expr

compile :: String -> Maybe ([Token], Expression)
compile content = do
  tokens <- tokenise content
  trace (show tokens) Just ()
  parseExpression (filter (not . isComment) tokens)
  where
    isComment :: Token -> Bool
    isComment (Comment _) = True
    isComment _ = False

readFile :: FilePath -> IO String
readFile p = do
  handle <- openFile p ReadMode
  hGetContents handle
