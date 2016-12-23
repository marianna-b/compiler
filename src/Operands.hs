{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module Operands where

import Codegeneration
import Types

import Control.Monad.State
import Data.Map as Map

import LLVM.General.AST

import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case Prelude.lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

local :: Type -> Name -> Operand
local = LocalReference

global :: Type -> Name -> C.Constant
global = C.GlobalReference

externf :: Type -> Name -> Operand
externf x = ConstantOperand . C.GlobalReference x

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = Prelude.map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn@(LocalReference t _) args = instr t $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []
call fn@(ConstantOperand (C.GlobalReference t _)) args = instr t $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []
call s _ = error $ "Not a local reference in call " ++ show s

alloca :: Type -> Codegen Operand
alloca ty = instr ty $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store pt@(LocalReference ty _) val = instr ty $ Store False pt val Nothing 0 []
store _ _ = error "Not a local reference in store"

load :: Operand -> Codegen Operand
load pt@(LocalReference ty _) = instr ty $ Load False pt Nothing 0 []
load _ = error "Not a local reference in load"

cons :: C.Constant -> Operand
cons = ConstantOperand

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr double $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr double $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr double $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr double $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr double $ FCmp cond a b []

iadd :: Operand -> Operand -> Codegen Operand
iadd a b = instr integer $ Add False False a b []

isub :: Operand -> Operand -> Codegen Operand
isub a b = instr integer $ Sub False False a b []

imul :: Operand -> Operand -> Codegen Operand
imul a b = instr integer $ Mul False False a b []

idiv :: Operand -> Operand -> Codegen Operand
idiv a b = instr integer $ UDiv False a b []

icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = instr integer $ ICmp cond a b []


stl :: Map.Map String (Operand -> Operand -> Codegen Operand)
stl = Map.fromList [
      ("iadd", iadd)
    , ("isub", isub)
    , ("imul", imul)
    , ("idiv", idiv)
    , ("ilt", (icmp IP.ULT))
    , ("igt", (icmp IP.UGT))
    , ("ieq", (icmp IP.EQ))
    , ("ineq", (icmp IP.NE))
    , ("fadd", fadd)
    , ("fsub", fsub)
    , ("fmul", fmul)
    , ("fdiv", fdiv)
    , ("flt", (fcmp FP.ULT))
    , ("fbt", (fcmp FP.UGT))
    , ("feq", (fcmp FP.UEQ))
    , ("fneq", (fcmp FP.UNE))
  ]
