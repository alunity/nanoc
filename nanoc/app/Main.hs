module Main where

import Data.Char (isDigit, isAlphaNum)
import Data.List (stripPrefix)
import System.IO hiding (readFile)
import Prelude hiding (readFile)
import Debug.Trace (trace)

main :: IO ()
main = do
  content <- readFile "main.c"
  let tokens = tokenise content
  case tokens of
    Just ts -> print ts
    Nothing -> print "Tokenisation failed"
  -- putStrLn content

readFile :: FilePath -> IO String
readFile p = do
  handle <- openFile p ReadMode
  hGetContents handle

data Token = Identifier String | Comment String | KwInt | Comma | LParen | RParen | LBrace | RBrace | Semicolon | Assign | Plus | Minus | EOF | IntLiteral Int deriving (Show)

-- character tokens
tokenise :: String -> Maybe [Token]
tokenise (',' : xs) = (Comma :) <$> tokenise xs
tokenise ('(' : xs) = (LParen :) <$> tokenise xs
tokenise (')' : xs) = (RParen :) <$> tokenise xs
tokenise ('{' : xs) = (LBrace :) <$> tokenise xs
tokenise ('}' : xs) = (RBrace :) <$> tokenise xs
tokenise (';' : xs) = (Semicolon :) <$> tokenise xs
tokenise ('=' : xs) = (Assign :) <$> tokenise xs -- problematic when we introduce == this is an issue
tokenise ('+' : xs) = (Plus :) <$> tokenise xs
tokenise ('-' : xs) = (Minus :) <$> tokenise xs
-- Skip whitespace
tokenise (' ' : xs) = tokenise xs
tokenise ('\n' : xs) = tokenise xs
tokenise [] = Just [EOF]
-- keywords
tokenise s
  | Just afterSlashes <- stripPrefix "//" s
  , Just (tok, xs) <- tokeniseWhile (/= '\n') Comment afterSlashes 
  = (tok: ) <$> tokenise xs
  | Just xs <- stripPrefix "int " s 
  = (KwInt :) <$> tokenise xs
  | Just (tok, xs) <- tokeniseIntLiteral s 
  = (tok :) <$> tokenise xs
  | Just (tok, xs) <- tokeniseIdentifier s 
  = (tok :) <$> tokenise xs
  | otherwise = trace s undefined 

tokeniseWhile :: (Char -> Bool) -> (String -> Token) -> String -> Maybe (Token, String)
tokeniseWhile p mktok s = let (identifier, rest) = span p s
   in if null identifier then Nothing else Just (mktok identifier, rest)

tokeniseIdentifier :: String -> Maybe (Token, String)
tokeniseIdentifier = tokeniseWhile isAlphaNum Identifier 

tokeniseIntLiteral :: String -> Maybe (Token, String)
tokeniseIntLiteral  = tokeniseWhile isDigit (IntLiteral . read)

