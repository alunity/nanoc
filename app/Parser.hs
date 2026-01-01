{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Parser (parseExpression) where

import Debug.Trace (trace)
import Lexer (Token)
import qualified Lexer as L
import Prelude hiding (GT, LT)

-- data AST = Atom Atom
newtype Program = Program [Function] deriving (Show)

data Function = Function
  { fRetType :: Type,
    fName :: String,
    fParams :: [(Type, String)],
    fBody :: [Statement]
  }
  deriving (Show)

data Type = TInt deriving (Show)

data Statement
  = SDeclare Type String (Maybe Expression)
  | SAssign String Expression
  | SExpr Expression
  | SIf Expression [Statement]
  | SWhile Expression [Statement]
  | SBlock [Statement]
  deriving (Show)

data Expression
  = EVar String
  | ELit Lit
  | ECall String [Expression]
  | EUOp UOp Expression
  | EBiOp BiOp Expression Expression
  deriving (Show)

data UOp = LNot | Negate deriving (Show)

data Lit = LInt Int deriving (Show)

data BiOp = LOr | LAnd | Eq | NEq | LT | LEq | GT | GEq | Add | Minus | Mul | Div deriving (Show)

type Parser a = ([Token] -> Maybe ([Token], a))

parseExpression :: Parser Expression
parseExpression = parseLOr

parseLOrSym :: Parser BiOp
parseLOrSym (L.LOr : ts) = Just (ts, LOr)
parseLOrSym _ = Nothing

parseLOr :: Parser Expression
parseLOr = parsePrecendentally parseLAnd parseLOrSym

parseLAndSym :: Parser BiOp
parseLAndSym (L.LAnd : ts) = Just (ts, LAnd)
parseLAndSym _ = Nothing

parseLAnd :: Parser Expression
parseLAnd = parsePrecendentally parseEq parseLAndSym

parseEqSym :: Parser BiOp
parseEqSym (L.Equality : ts) = Just (ts, Eq)
parseEqSym (L.NEquality : ts) = Just (ts, NEq)
parseEqSym _ = Nothing

parseEq :: Parser Expression
parseEq = parsePrecendentally parseRe parseEqSym

parseReSym :: Parser BiOp
parseReSym (L.GT : ts) = Just (ts, GT)
parseReSym (L.GEQ : ts) = Just (ts, GEq)
parseReSym (L.LT : ts) = Just (ts, LT)
parseReSym (L.LEQ : ts) = Just (ts, LEq)
parseReSym _ = Nothing

parseRe :: Parser Expression
parseRe = parsePrecendentally parseAdd parseReSym

parseAddSym :: Parser BiOp
parseAddSym (L.Plus : ts) = Just (ts, Add)
parseAddSym (L.Minus : ts) = Just (ts, Minus)
parseAddSym _ = Nothing

parseAdd :: Parser Expression
parseAdd = parsePrecendentally parseMul parseAddSym

parseMulSym :: Parser BiOp
parseMulSym (L.Multiply : ts) = Just (ts, Mul)
parseMulSym (L.Divide : ts) = Just (ts, Div)
parseMulSym _ = Nothing

parseMul :: Parser Expression
parseMul = parsePrecendentally parseUnary parseMulSym

parseUnary :: Parser Expression
parseUnary ts
  | Just (ts', op) <- parseUop ts = do
      (ts'', e) <- parseExpression ts'
      return (ts'', EUOp op e)
  | otherwise = parseAtom ts

parseAtom :: Parser Expression
parseAtom ((L.IntLiteral d) : ts) = Just (ts, ELit (LInt d))
parseAtom ((L.Identifier d) : L.LParen : ts) = do
  (ts', es) <- parseMultiple (Just L.Comma) L.RParen parseExpression ts
  return (ts', ECall d es)
parseAtom ((L.Identifier d) : ts) = Just (ts, EVar d)
parseAtom ts@(L.LParen : _) = between L.LParen L.RParen parseExpression ts
parseAtom _ = Nothing

parseUop :: Parser UOp
parseUop (L.LNot : ts) = Just (ts, LNot)
parseUop (L.Minus : ts) = Just (ts, Negate)
parseUop _ = Nothing

expect :: Token -> Parser ()
expect t (x : xs)
  | t == x = Just (xs, ())
  | otherwise = Nothing
expect _ _ = Nothing

between :: Token -> Token -> Parser a -> Parser a
between o c p ts = do
  (ts', _) <- expect o ts
  (ts'', a) <- p ts'
  (ts''', _) <- expect c ts''
  Just (ts''', a)

parseMultiple :: Maybe Token -> Token -> Parser a -> Parser [a]
parseMultiple Nothing end p (t : ts)
  | t == end = Just (ts, [])
  | otherwise = do
      (ts', e) <- p (t : ts)
      (ts'', es) <- parseMultiple Nothing end p ts'
      return (ts'', e : es)
parseMultiple (Just d) end p (t : ts)
  | t == end = Just (ts, [])
  | otherwise = do
      (ts', a) <- p (t : ts)
      (ts'', as) <- divOrEnd ts'
      return (ts'', a : as)
  where
    divOrEnd (x : xs)
      | x == end = Just (xs, [])
      | x == d = parseMultiple (Just d) end p xs
      | otherwise = Nothing
    divOrEnd _ = Nothing
parseMultiple _ _ _ _ = trace "Hi" Nothing

parsePrecendentally :: Parser Expression -> Parser BiOp -> Parser Expression
parsePrecendentally p op ts = do
  (ts', l) <- p ts
  return (parsePrecendentally' l op p ts')
  where
    parsePrecendentally' :: Expression -> Parser BiOp -> Parser Expression -> [Token] -> ([Token], Expression)
    parsePrecendentally' e op p ts = case allowedToFail of
      Just (sym, r, ts'') -> parsePrecendentally' (EBiOp sym e r) op p ts''
      Nothing -> (ts, e)
      where
        allowedToFail = do
          (ts', sym) <- op ts
          (ts'', r) <- p ts'
          return (sym, r, ts'')
