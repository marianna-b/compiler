{-# OPTIONS -Wall #-}
module Types where

import LLVM.General.AST.Type

integer :: Type
integer = i64

double :: Type
double = FloatingPointType 64 IEEE

