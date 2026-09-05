module Codegen where

import Control.Monad.State
import Data.Map as Map
import Parser (Expression (..), Function (fBody, fName), Lit (..), Program (..), Statement (..), UOp (LNot, Negate))

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
  show (Mul r1 r2 r3) = "mul " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show r3)
  show (Nor r1 r2 r3) = "nor " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show r3)
  show (Lw r1 i r2) = "lw " ++ (show r1) ++ "," ++ (show i) ++ "(" ++ show (r2) ++ ")"
  show (Sw r1 i r2) = "sw " ++ (show r1) ++ "," ++ (show i) ++ "(" ++ show (r2) ++ ")"
  show (Addiu r1 r2 i) = "addu " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show i)
  show (Subiu r1 r2 i) = "subu " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show i)
  show (Beq r1 r2 s) = "beq " ++ (show r1) ++ "," ++ (show r2) ++ "," ++ (show s)
  show (J s) = "j " ++ (show s)
  show (Jr r) = "jr " ++ (show r)
  show (Jal s) = "jal " ++ (show s)
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
  put s {currentEnd = '.' : (fName f) ++ "_end", env = Map.empty}
  emit $ Label (fName f)
  -- figure out stack frame
  mapM_ genStatement (fBody f)
  genFunctionEpilogue
  where
    genFunctionEpilogue :: Codegen ()
    genFunctionEpilogue = do
      s <- get
      emit $ Label (currentEnd s)
      emit $ Jr RA

genStatement :: Statement -> Codegen ()
genStatement (SExpr e) = genExpression e
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

genExpression :: Expression -> Codegen ()
genExpression (ELit l) = genLiteral l
genExpression (ECall "outInt" [ELit (LInt n)]) = do
  emit $ Li V0 1
  emit $ Li A0 n
  emit $ Syscall
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