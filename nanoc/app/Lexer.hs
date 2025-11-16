module Lexer where

import Data.List (stripPrefix)
import Debug.Trace (trace)
import GHC.Unicode

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
tokenise (c: xs) | isSpace c = tokenise xs
tokenise [] = Just [EOF]
-- keywords
tokenise s
  | Just afterSlashes <- stripPrefix "//" s,
    Just (tok, xs) <- tokeniseWhile (/= '\n') Comment afterSlashes =
      (tok :) <$> tokenise xs
  | Just (tok, xs) <- tokeniseIntLiteral s =
      (tok :) <$> tokenise xs
  | Just (tok, xs) <- tokeniseKeywordOrIdentifier s =
      (tok :) <$> tokenise xs
  | otherwise = trace s undefined

tokeniseIdentifier :: String -> Maybe (String, String)
tokeniseIdentifier (x:xs)
  | isAlpha x || x == '_' =
    let (tailChars, rest) = span (\char -> isAlphaNum char || char == '_') xs
    in Just (x:tailChars, rest)
tokeniseIdentifier _ = Nothing

tokeniseKeywordOrIdentifier :: String -> Maybe (Token, String)
tokeniseKeywordOrIdentifier s = do
  (lexeme, rest) <- tokeniseIdentifier s
  pure (keyword lexeme, rest)
  where 
    keyword "int" = KwInt
    keyword x = Identifier x

tokeniseWhile :: (Char -> Bool) -> (String -> Token) -> String -> Maybe (Token, String)
tokeniseWhile p mktok s =
  let (identifier, rest) = span p s
   in if null identifier then Nothing else Just (mktok identifier, rest)

tokeniseIntLiteral :: String -> Maybe (Token, String)
tokeniseIntLiteral = tokeniseWhile isDigit (IntLiteral . read)
