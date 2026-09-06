module Codegen where

import Control.Monad.State
import Data.Map as Map
import Debug.Trace (traceM)
import Parser (BiOp (Add, Divide, Eq, GEq, GT, LAnd, LEq, LOr, LT, Minus, Multiply, NEq), Expression (..), Function (fBody, fName, fParams), Lit (..), Program (..), Statement (..), Type, UOp (LNot, Negate))
import Prelude hiding (GT, LT)

data Reg = T0 | T1 | V0 | A0 | SP | FP | RA | ZERO

instance Show Reg where
  show T0 = "$t0"
  show T1 = "$t1"
  show V0 = "$v0"
  show A0 = "$a0"
  show SP = "$sp"
  show FP = "$fp"
  show RA = "$ra"
  show ZERO = "$0"

data Instr
  = Li Reg Int
  | Move Reg Reg
  | Addu Reg Reg Reg
  | Subu Reg Reg Reg
  | Mul Reg Reg Reg
  | Div Reg Reg Reg
  | And Reg Reg Reg
  | Or Reg Reg Reg
  | Slt Reg Reg Reg
  | Sle Reg Reg Reg
  | Seq Reg Reg Reg
  | Sne Reg Reg Reg
  | Nor Reg Reg Reg
  | Lw Reg Int Reg -- Lw rt offset(base)
  | Sw Reg Int Reg -- Sw rt offset(base)
  | Addiu Reg Reg Int -- Psuedoinstruction
  | Subiu Reg Reg Int
  | Beq Reg Reg String
  | J String
  | Jal String
  | Jr Reg
  | Label String
  | Syscall

instance Show Instr where
  show (Li r i) = "li " ++ (show r) ++ "," ++ (show i)
  show (Move r1 r2) = "move " ++ (show r1) ++ "," ++ (show r2)
  show (Addu r1 r2 r3) = "add " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show r3)
  show (Subu r1 r2 r3) = "sub " ++ (show r1) ++ "," ++ (show r2) ++ ",," ++ (show r3)
  show (And r1 r2 r3) = "and " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show r3)
  show (Slt r1 r2 r3) = "slt " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show r3)
  show (Sle r1 r2 r3) = "sle " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show r3)
  show (Seq r1 r2 r3) = "seq " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show r3)
  show (Sne r1 r2 r3) = "sne " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show r3)
  show (Mul r1 r2 r3) = "mul " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show r3)
  show (Div r1 r2 r3) = "div " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show r3)
  show (Nor r1 r2 r3) = "nor " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show r3)
  show (Or r1 r2 r3) = "or " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show r3)
  show (Lw r1 i r2) = "lw " ++ (show r1) ++ "," ++ (show i) ++ "(" ++ show (r2) ++ ")"
  show (Sw r1 i r2) = "sw " ++ (show r1) ++ "," ++ (show i) ++ "(" ++ show (r2) ++ ")"
  show (Addiu r1 r2 i) = "addu " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show i)
  show (Subiu r1 r2 i) = "subu " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show i)
  show (Beq r1 r2 s) = "beq " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (s)
  show (J s) = "j " ++ (s)
  show (Jr r) = "jr " ++ (show r)
  show (Jal s) = "jal " ++ (s)
  show (Label s) = s ++ ":"
  show (Syscall) = "syscall"

data GenState = GenState
  { labelCount :: Int,
    env :: Map String Int, -- maps variable name to offset from $fp
    currentEnd :: String, -- label of the current function's epilogue
    revCode :: [Instr] -- emitted instructions stored in reverse (O(1) prepend)
  }

type Codegen a = State GenState a

emit :: Instr -> Codegen ()
emit instr = modify (\s -> s {revCode = instr : revCode s})

freshLabel :: String -> Codegen String
freshLabel prefix = do
  s <- get
  let n = labelCount s
  put s {labelCount = n + 1}
  pure $ prefix ++ "_" ++ show n

push :: Reg -> Codegen ()
push r = do
  emit $ Subiu SP SP 4
  emit $ Sw r 0 SP

pop :: Reg -> Codegen ()
pop r = do
  emit $ Lw r 0 SP
  emit $ Addiu SP SP 4

genFunction :: Function -> Codegen ()
genFunction f = do
  s <- get
  put s {currentEnd = '.' : (fName f) ++ "_end", env = (fullMap)}
  traceM (show fullMap)
  emit $ Label (fName f)
  genFunctionPrologue
  -- figure out stack frame
  genStatements (fBody f)
  genFunctionEpilogue
  where
    mapArguments :: [(Type, String)] -> Int -> Map String Int -> Map String Int
    mapArguments [] _ m = m
    mapArguments ((_, s) : xs) i m = mapArguments xs (i + 4) (Map.insert s i m)

    mapLocals :: [(Type, String)] -> Int -> Map String Int -> (Map String Int, Int)
    mapLocals [] i m = (m, i + 4)
    mapLocals ((_, s) : xs) i m = mapLocals xs (i - 4) (Map.insert s i m)

    findLocals :: [Statement] -> [(Type, String)]
    findLocals (s : xs) = (dfs s) ++ (findLocals xs)
      where
        dfs :: Statement -> [(Type, String)]
        dfs (SDeclare t name _) = [(t, name)]
        dfs (SIf _ ss) = concat (Prelude.map dfs ss)
        dfs (SWhile _ ss) = concat (Prelude.map dfs ss)
        dfs (SBlock ss) = concat (Prelude.map dfs ss)
        dfs _ = []
    findLocals [] = []

    argumentMap = mapArguments (reverse (fParams f)) 8 Map.empty
    (fullMap, stackStart) = mapLocals (findLocals (fBody f)) (-4) argumentMap

    genFunctionPrologue :: Codegen ()
    genFunctionPrologue = do
      emit $ Addiu SP SP (-8)
      emit $ Sw RA 4 SP
      emit $ Sw FP 0 SP
      emit $ Move FP SP
      emit $ Addiu SP SP stackStart

    genFunctionEpilogue :: Codegen ()
    genFunctionEpilogue = do
      s <- get
      emit $ Label (currentEnd s)
      emit $ Jr RA

lookupVar :: String -> Codegen Int
lookupVar s = do
  envMap <- gets env
  case Map.lookup s envMap of
    Just offset -> pure offset
    Nothing -> undefined

genStatements :: [Statement] -> Codegen ()
genStatements = mapM_ genStatement

genStatement :: Statement -> Codegen ()
genStatement (SExpr e) = genExpression e
genStatement (SDeclare _ _ Nothing) = pure ()
genStatement (SDeclare _ s (Just e)) = genStatement (SAssign s e)
genStatement (SAssign s e) = do
  genExpression e
  pop T0
  offset <- lookupVar s
  emit $ Sw T0 offset FP
genStatement (SIf e ss) = do
  genExpression e
  pop T0
  end <- freshLabel "end"
  emit $ Beq T0 ZERO end
  genStatements ss
  emit $ Label end
genStatement (SWhile e ss) = do
  start <- freshLabel "start"
  end <- freshLabel "end"
  emit $ Label start
  genExpression e
  pop T0
  emit $ Beq T0 ZERO end
  genStatements ss
  emit $ J start
  emit $ Label end
genStatement _ = undefined

genLiteral :: Lit -> Codegen ()
genLiteral (LInt i) = do
  emit $ Li T0 i
  push T0

genUOp :: UOp -> Codegen ()
genUOp v = do
  pop T0
  case v of
    LNot -> emit $ Nor T0 T0 T0
    Negate -> emit $ Subu T0 ZERO T0
  push T0

genBiOp :: BiOp -> Codegen ()
genBiOp v = do
  pop T1
  pop T0
  case v of
    LOr -> emit $ Or T0 T0 T1
    LAnd -> emit $ And T0 T0 T1
    Eq -> emit $ Seq T0 T0 T1
    NEq -> emit $ Sne T0 T0 T1
    LT -> emit $ Slt T0 T0 T1
    LEq -> emit $ Sle T0 T0 T1
    GT -> emit $ Slt T0 T1 T0
    GEq -> emit $ Sle T0 T1 T0
    Add -> emit $ Addu T0 T0 T1
    Minus -> emit $ Subu T0 T0 T1
    Multiply -> emit $ Mul T0 T0 T1
    Divide -> emit $ Div T0 T0 T1
  push T0

genExpression :: Expression -> Codegen ()
genExpression (ELit l) = genLiteral l
genExpression (EUOp uop expression) = do
  genExpression expression
  genUOp uop
genExpression (EBiOp biop e1 e2) = do
  genExpression e1
  genExpression e2
  genBiOp biop
genExpression (ECall "outInt" [expression]) = do
  genExpression expression
  pop A0
  emit $ Li V0 1
  emit $ Syscall
  emit $ Li V0 11
  emit $ Li A0 10
  emit $ Syscall
genExpression (ECall "readInt" []) = do
  emit $ Li V0 5
  emit $ Syscall
  push V0
genExpression (EVar s) = do
  offset <- lookupVar s
  emit $ Lw T0 offset FP
  push T0
genExpression _ = undefined

genProgram :: Program -> Codegen ()
genProgram (Program fs) = mapM_ genFunction fs

runCodegen :: Codegen () -> [Instr]
runCodegen action =
  let initialState =
        GenState
          { labelCount = 0,
            env = Map.empty,
            currentEnd = "",
            revCode = []
          }
      finalState = execState action initialState
   in reverse (revCode finalState)

codegen :: Program -> [Instr]
codegen program = runCodegen (genProgram program)