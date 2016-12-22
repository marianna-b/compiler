{-# LANGUAGE OverloadedStrings #-}
module IR where

import qualified LLVM.General.Module as M
import qualified LLVM.General.Context as CTX

import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.IntegerPredicate as IP

import qualified Data.Word
import qualified Data.Int
import Data.Traversable(traverse)
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegeneration
import qualified AST as A

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (integer, AST.Name x))

codegenTop :: A.TopLevelDecl -> LLVM ()
codegenTop (A.FuncDecl name args body)
  | name == "main" = codegenMain body
  | otherwise = do
      define integer name fnargs bls
        where
          fnargs = toSig args
          bls = createBlocks $ execCodegen $ do
            entry <- addBlock entryBlockName
            setBlock entry
            forM args $ \a -> do
              var <- alloca integer
              store var (local (AST.Name a))
              assign a var
            traverse (\x -> cgen x >>= ret) body

codegenTop (A.ExternFunc name args) = do
  external integer name fnargs
  where fnargs = toSig args

codegenMain :: [A.Expr] -> LLVM ()
codegenMain body = do
  define integer "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      traverse (\x -> cgen x >>= ret) body

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- icmp IP.ULT a b
  return test

binops = Map.fromList [
      ("+", add)
    , ("-", sub)
    , ("*", mul)
    , ("/", idiv)
    , ("<", lt)
  ]


cgen :: A.Expr -> Codegen AST.Operand
--cgen (UnaryOp op a) = do
--  cgen $ Call ("unary" ++ op) [a]
--cgen (BinaryOp "=" (Var var) val) = do
--  a <- getvar var
--  cval <- cgen val
--  store a cval
--  return cval
--cgen (BinaryOp op a b) = do
--  case Map.lookup op binops of
--    Just f  -> do
--      ca <- cgen a
--      cb <- cgen b
--      f ca cb
--    Nothing -> error "No such operator"
cgen (A.Binding x) = getvar x >>= load
cgen (A.Literal (A.IntegerLiteral n)) = return $ cons $ C.Int 64 n
cgen (A.FuncCall  (A.Binding fn:args)) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> A.Module -> IO AST.Module
codegen mod fns = CTX.withContext $ \context ->
  liftError $ M.withModuleFromAST context newast $ \m -> do
    llstr <- M.moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
