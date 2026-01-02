module Main where

import Lexer (Token (Comment), tokenise)
import Parser (Program, parse)
import System.IO hiding (readFile)
import Prelude hiding (readFile)
import Debug.Trace (trace)

main :: IO ()
main = do
  content <- readFile "./local/main.c"
  case compile content of
    Left err -> putStrLn err
    Right prog -> print prog

compile :: String -> Either String ([Token], Program)
compile content = do
  tokens <- maybeToEither "Tokenisation failed" (tokenise content)
  trace (show tokens) Right ()
  parse (filter (not . isComment) tokens)
  where
    isComment :: Token -> Bool
    isComment (Comment _) = True
    isComment _ = False

readFile :: FilePath -> IO String
readFile p = do
  handle <- openFile p ReadMode
  hGetContents handle

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err = maybe (Left err) Right