{-# OPTIONS -Wall #-}
module IR where

import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.Module as M
import qualified LLVM.General.Context as CTX
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

import Control.Monad.Except
import Control.Monad.State
import Data.Map as Map

import Types
import LLVM
import Codegeneration
import Operands
import qualified AST as A

typed :: String -> AST.Type
typed "int" = integer
typed "double" = double
typed s = error $ "Unknown type" ++ s

parseType :: (String, String) -> (AST.Type, AST.Name)
parseType (t, x) = (typed t, AST.Name x)

toSig :: [(String, String)] -> [(AST.Type, AST.Name)]
toSig = Prelude.map parseType

codegenTop :: A.TopLevelDecl -> LLVM ()
codegenTop (A.FuncDecl tn args body) = do
    defs <- gets AST.moduleDefinitions
    define t name fnargs $ bls defs
  where
    (t, name) = parseType tn
    fnargs = toSig args
    bls defs = createBlocks $ execCodegen $ do
      let types = getTypes defs
      entr <- addBlock entryBlockName
      _ <- setBlock entr
      _ <- setFuncTypes types
      _ <- ($) forM args $ \(t1, a) -> do
        let ty = typed t1
        var <- alloca ty
        _ <- store var (local ty (AST.Name a))
        assign a var
      traverse (\x -> cgen x >>= ret) body

codegenTop (A.ExternFunc tn args) = do
  external t name fnargs
  where
    fnargs = toSig args
    (t, name) = parseType tn

exec :: (AST.Operand -> AST.Operand -> Codegen AST.Operand) -> A.Expr -> A.Expr -> Codegen AST.Operand
exec f x y = do
  a <- cgen x
  b <- cgen y
  f a b

nonStlGen :: A.Expr -> Codegen AST.Operand
nonStlGen (A.FuncCall  (A.Binding fn:args)) = do
  t <- getFuncType $ AST.Name fn
  largs <- mapM cgen args
  call (externf t (AST.Name fn)) largs
nonStlGen _ = error "Unsupported expression"

cgen :: A.Expr -> Codegen AST.Operand
cgen (A.Binding x) = getvar x >>= load
cgen (A.Literal (A.IntegerLiteral n)) = return $ cons $ C.Int 64 n
cgen (A.IF cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  cnd <- cgen cond
  test <- icmp IP.NE (cons $ C.Int 1 0) cnd
  _ <- cbr test ifthen ifelse

  _ <- setBlock ifthen
  trval <- cgen tr
  _ <- br ifexit
  ifthen' <- getBlock

  _ <- setBlock ifelse
  flval <- cgen fl
  _ <- br ifexit
  ifelse' <- getBlock

  _ <- setBlock ifexit
  phi integer [(trval, ifthen'), (flval, ifelse')]

cgen (A.For (t, ivar) start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"

  i <- alloca $ typed t
  istart <- cgen start
  stepval <- cgen step

  _ <- store i istart
  assign ivar i
  _ <- br forloop

  _ <- setBlock forloop
  _ <- cgen body
  ival <- load i
  inext <- case t of
             "int" -> iadd ival stepval
             "double" -> fadd ival stepval
             _ -> error $ "Non supported type" ++ t
  _ <- store i inext

  cnd <- cgen cond
  test <- icmp IP.NE (cons $ C.Int 1 0) cnd
  _ <- cbr test forloop forexit

  _ <- setBlock forexit
  return (cons $ C.Int 64 0)
cgen (A.Let (t, a) b c) = do
  i <- alloca $ typed t
  val <- cgen b
  _ <- store i val
  assign a i
  cgen c
cgen func@(A.FuncCall (A.Binding s:[x, y])) =
  case Map.lookup s stl of
    Just f -> exec f x y
    Nothing -> nonStlGen func
cgen x = nonStlGen x

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> A.Module -> IO AST.Module
codegen md fns = CTX.withContext $ \context ->
  liftError $ M.withModuleFromAST context newast $ \m -> do
    llstr <- M.moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM md modn


