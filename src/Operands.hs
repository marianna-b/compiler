{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module Operands where

import Codegeneration

import Control.Monad.State

import LLVM.General.AST

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
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

local :: Type -> Name -> Operand
local = LocalReference

global :: Type -> Name -> C.Constant
global = C.GlobalReference

externf :: Type -> Name -> Operand
externf x = ConstantOperand . C.GlobalReference x

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

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
