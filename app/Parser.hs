{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser (parse, Program(..), Function(..), Statement(..), Expression(..), Lit(..), UOp(..), BiOp(..)) where

import qualified Data.Bifunctor
import Lexer (Token)
import qualified Lexer as L
import Prelude hiding (GT, LT)

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

type Parser a = ([Token] -> Either String ([Token], a))

parse :: Parser Program
parse ts = do
  (ts', fs) <- parseFunctions ts
  return (ts', Program fs)

parseFunctions :: Parser [Function]
parseFunctions = parseMultiple Nothing L.EOF parseFunction

parseFunction :: Parser Function
parseFunction ts = do
  (ts', t) <- parseType ts
  (ts'', i) <- parseIdentifier ts'
  (ts''', _) <- expect L.LParen ts''
  (ts'''', args) <- parseMultiple (Just L.Comma) L.RParen parseArg ts'''
  (ts''''', body) <- parseScope ts''''
  Right (ts''''', Function {fRetType = t, fName = i, fParams = args, fBody = body})

parseType :: Parser Type
parseType (L.KwInt : ts) = Right (ts, TInt)
parseType a = Left (expectedError "Type" a)

parseIdentifier :: Parser String
parseIdentifier ((L.Identifier d) : ts) = Right (ts, d)
parseIdentifier a = Left (expectedError "Identifer" a)

parseArg :: Parser (Type, String)
parseArg ts = do
  (ts', t) <- parseType ts
  (ts'', i) <- parseIdentifier ts'
  return (ts'', (t, i))

parseScope :: Parser [Statement]
parseScope ts = do
  (ts', _) <- expect L.LBrace ts
  parseMultiple Nothing L.RBrace parseStatement ts'

-- Statement

parseStatement :: Parser Statement
parseStatement ts = parseSimpleStatement ts `orElse` parseComplexStatement ts

parseSimpleStatement :: Parser Statement
parseSimpleStatement ts = do
  (ts', s) <- aux
  (ts'', _) <- expect L.Semicolon ts'
  return (ts'', s)
  where
    aux =
      parseDeclarationAssignment ts
        `orElse` parseDeclaration ts
        `orElse` parseAssignment ts
        `orElse` (Data.Bifunctor.second SExpr <$> parseExpression ts)

parseComplexStatement :: Parser Statement
parseComplexStatement (L.KwIf : ts) = do
  (ts', e) <- between L.LParen L.RParen parseExpression ts
  (ts'', s) <- parseScope ts'
  return (ts'', SIf e s)
parseComplexStatement (L.KwWhile : ts) = do
  (ts', e) <- between L.LParen L.RParen parseExpression ts
  (ts'', s) <- parseScope ts'
  return (ts'', SWhile e s)
parseComplexStatement ts@(L.LBrace : _) = do
  (ts', s) <- parseScope ts
  return (ts', SBlock s)
parseComplexStatement ts = Left ("Failed to parse statement " ++ show ts)

parseDeclarationAssignment :: Parser Statement
parseDeclarationAssignment ts = do
  (ts', (t, i)) <- parseArg ts
  (ts'', _) <- expect L.Assign ts'
  (ts''', e) <- parseExpression ts''
  return (ts''', SDeclare t i (Just e))

parseDeclaration :: Parser Statement
parseDeclaration ts = (\(ts, (t, i)) -> (ts, SDeclare t i Nothing)) <$> parseArg ts

parseAssignment :: Parser Statement
parseAssignment ts = do
  (ts', i) <- parseIdentifier ts
  (ts'', _) <- expect L.Assign ts'
  (ts''', e) <- parseExpression ts''
  return (ts''', SAssign i e)

-- Expression

parseExpression :: Parser Expression
parseExpression = parseLOr

parseLOrSym :: Parser BiOp
parseLOrSym (L.LOr : ts) = Right (ts, LOr)
parseLOrSym a = Left (expectedError "LOr" a)

parseLOr :: Parser Expression
parseLOr = parsePrecendentally parseLAnd parseLOrSym

parseLAndSym :: Parser BiOp
parseLAndSym (L.LAnd : ts) = Right (ts, LAnd)
parseLAndSym a = Left (expectedError "LAnd" a)

parseLAnd :: Parser Expression
parseLAnd = parsePrecendentally parseEq parseLAndSym

parseEqSym :: Parser BiOp
parseEqSym (L.Equality : ts) = Right (ts, Eq)
parseEqSym (L.NEquality : ts) = Right (ts, NEq)
parseEqSym a = Left (expectedError "Eq" a)

parseEq :: Parser Expression
parseEq = parsePrecendentally parseRe parseEqSym

parseReSym :: Parser BiOp
parseReSym (L.GT : ts) = Right (ts, GT)
parseReSym (L.GEQ : ts) = Right (ts, GEq)
parseReSym (L.LT : ts) = Right (ts, LT)
parseReSym (L.LEQ : ts) = Right (ts, LEq)
parseReSym a = Left (expectedError "Re" a)

parseRe :: Parser Expression
parseRe = parsePrecendentally parseAdd parseReSym

parseAddSym :: Parser BiOp
parseAddSym (L.Plus : ts) = Right (ts, Add)
parseAddSym (L.Minus : ts) = Right (ts, Minus)
parseAddSym a = Left (expectedError "Add" a)

parseAdd :: Parser Expression
parseAdd = parsePrecendentally parseMul parseAddSym

parseMulSym :: Parser BiOp
parseMulSym (L.Multiply : ts) = Right (ts, Mul)
parseMulSym (L.Divide : ts) = Right (ts, Div)
parseMulSym a = Left (expectedError "Mul" a)

parseMul :: Parser Expression
parseMul = parsePrecendentally parseUnary parseMulSym

parseUnary :: Parser Expression
parseUnary ts
  | Right (ts', op) <- parseUOp ts = do
      (ts'', e) <- parseExpression ts'
      return (ts'', EUOp op e)
  | otherwise = parseAtom ts

parseAtom :: Parser Expression
parseAtom ((L.IntLiteral d) : ts) = Right (ts, ELit (LInt d))
parseAtom ((L.Identifier d) : L.LParen : ts) = do
  (ts', es) <- parseMultiple (Just L.Comma) L.RParen parseExpression ts
  return (ts', ECall d es)
parseAtom ((L.Identifier d) : ts) = Right (ts, EVar d)
parseAtom ts@(L.LParen : _) = between L.LParen L.RParen parseExpression ts
parseAtom a = Left (expectedError "Atom" a)

parseUOp :: Parser UOp
parseUOp (L.LNot : ts) = Right (ts, LNot)
parseUOp (L.Minus : ts) = Right (ts, Negate)
parseUOp a = Left (expectedError "UOp" a)

expect :: Token -> Parser ()
expect t (x : xs)
  | t == x = Right (xs, ())
  | otherwise = Left (expectedError (show t) (x : xs))
expect _ _ = Left "Expect failed"

between :: Token -> Token -> Parser a -> Parser a
between o c p ts = do
  (ts', _) <- expect o ts
  (ts'', a) <- p ts'
  (ts''', _) <- expect c ts''
  Right (ts''', a)

parseMultiple :: Maybe Token -> Token -> Parser a -> Parser [a]
parseMultiple Nothing end p (t : ts)
  | t == end = Right (ts, [])
  | otherwise = do
      (ts', e) <- p (t : ts)
      (ts'', es) <- parseMultiple Nothing end p ts'
      return (ts'', e : es)
parseMultiple (Just d) end p (t : ts)
  | t == end = Right (ts, [])
  | otherwise = do
      (ts', a) <- p (t : ts)
      (ts'', as) <- divOrEnd ts'
      return (ts'', a : as)
  where
    divOrEnd (x : xs)
      | x == end = Right (xs, [])
      | x == d = parseMultiple (Just d) end p xs
      | otherwise = Left (expectedError (show end ++ " or " ++ show d) (x : xs))
    divOrEnd _ = Left "parseMultiple expected more tokens"
parseMultiple _ _ _ _ = Left "parseMultiple failed"

parsePrecendentally :: Parser Expression -> Parser BiOp -> Parser Expression
parsePrecendentally p op ts = do
  (ts', l) <- p ts
  return (parsePrecendentally' l op p ts')
  where
    parsePrecendentally' :: Expression -> Parser BiOp -> Parser Expression -> [Token] -> ([Token], Expression)
    parsePrecendentally' e op p ts = case allowedToFail of
      Right (sym, r, ts'') -> parsePrecendentally' (EBiOp sym e r) op p ts''
      Left _ -> (ts, e)
      where
        allowedToFail = do
          (ts', sym) <- op ts
          (ts'', r) <- p ts'
          return (sym, r, ts'')

expectedError :: (Show a) => String -> a -> String
expectedError expected received = "Expected: " ++ expected ++ ", Received:" ++ show received

orElse :: Either e a -> Either e a -> Either e a
orElse (Right x) _ = Right x
orElse (Left _) r = r