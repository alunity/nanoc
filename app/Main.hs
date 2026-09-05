module Main where

import Codegen (codegen)
import Debug.Trace (trace)
import Lexer (Token (Comment), tokenise)
import Parser (parse)
import System.IO hiding (readFile)
import Prelude hiding (readFile)

main :: IO ()
main = do
  content <- readFile "./local/main.c"
  case compile content of
    Left err -> putStrLn err
    Right out -> writeFile "out/out.s" out

compile :: String -> Either String String
compile content = do
  tokens <- maybeToEither "Tokenisation failed" (tokenise content)
  trace (show tokens) Right ()
  (_, program) <- parse (filter (not . isComment) tokens)
  trace (show program) Right ()
  Right (unlines (prelude ++ ((map show) (codegen program))))
  where
    isComment :: Token -> Bool
    isComment (Comment _) = True
    isComment _ = False
    prelude = [".text", ".globl main"]

readFile :: FilePath -> IO String
readFile p = do
  handle <- openFile p ReadMode
  hGetContents handle

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err = maybe (Left err) Right