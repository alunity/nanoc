module Lexer where

import Control.Applicative ((<|>))
import Debug.Trace (trace)
import GHC.Unicode (isAlpha, isAlphaNum, isDigit, isSpace)
import Prelude hiding (GT, LT)

data Token = Identifier String | Comment String | KwInt | Comma | LParen | RParen | LBrace | RBrace | Semicolon | Assign | Plus | Minus | Multiply | Divide | EOF | IntLiteral Int | LAnd | LOr | LNot | Equality | NEquality | GT | LT | GEQ | LEQ | KwIf | KwWhile deriving (Show, Eq)

tokenise :: String -> Maybe [Token]
-- multicharacter tokens
tokenise ('&' : '&' : xs) = (LAnd :) <$> tokenise xs
tokenise ('|' : '|' : xs) = (LOr :) <$> tokenise xs
tokenise ('=' : '=' : xs) = (Equality :) <$> tokenise xs
tokenise ('!' : '=' : xs) = (NEquality :) <$> tokenise xs
tokenise ('>' : '=' : xs) = (GEQ :) <$> tokenise xs
tokenise ('<' : '=' : xs) = (LEQ :) <$> tokenise xs
tokenise ('/' : '/' : xs) = do
  (tok, xs') <- tokeniseWhile (/= '\n') Comment xs <|> Just (Comment "", xs)
  (tok :) <$> tokenise xs'
-- character tokens
tokenise ('!' : xs) = (LNot :) <$> tokenise xs
tokenise ('>' : xs) = (GT :) <$> tokenise xs
tokenise ('<' : xs) = (LT :) <$> tokenise xs
tokenise (',' : xs) = (Comma :) <$> tokenise xs
tokenise ('(' : xs) = (LParen :) <$> tokenise xs
tokenise (')' : xs) = (RParen :) <$> tokenise xs
tokenise ('{' : xs) = (LBrace :) <$> tokenise xs
tokenise ('}' : xs) = (RBrace :) <$> tokenise xs
tokenise (';' : xs) = (Semicolon :) <$> tokenise xs
tokenise ('=' : xs) = (Assign :) <$> tokenise xs
tokenise ('+' : xs) = (Plus :) <$> tokenise xs
tokenise ('-' : xs) = (Minus :) <$> tokenise xs
tokenise ('*' : xs) = (Multiply :) <$> tokenise xs
tokenise ('/' : xs) = (Divide :) <$> tokenise xs
-- Skip whitespace
tokenise (c : xs) | isSpace c = tokenise xs
tokenise [] = Just [EOF]
-- keywords
tokenise s
  | Just (tok, xs) <- tokeniseIntLiteral s =
      (tok :) <$> tokenise xs
  | Just (tok, xs) <- tokeniseKeywordOrIdentifier s =
      (tok :) <$> tokenise xs
  | otherwise = trace s undefined

tokeniseIdentifier :: String -> Maybe (String, String)
tokeniseIdentifier (x : xs)
  | isAlpha x || x == '_' =
      let (tailChars, rest) = span (\char -> isAlphaNum char || char == '_') xs
       in Just (x : tailChars, rest)
tokeniseIdentifier _ = Nothing

tokeniseKeywordOrIdentifier :: String -> Maybe (Token, String)
tokeniseKeywordOrIdentifier s = do
  (lexeme, rest) <- tokeniseIdentifier s
  pure (keyword lexeme, rest)
  where
    keyword "int" = KwInt
    keyword "if" = KwIf
    keyword "while" = KwWhile
    keyword x = Identifier x

tokeniseWhile :: (Char -> Bool) -> (String -> Token) -> String -> Maybe (Token, String)
tokeniseWhile p mktok s =
  let (identifier, rest) = span p s
   in if null identifier then Nothing else Just (mktok identifier, rest)

tokeniseIntLiteral :: String -> Maybe (Token, String)
tokeniseIntLiteral = tokeniseWhile isDigit (IntLiteral . read)
