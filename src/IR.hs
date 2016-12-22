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

typed :: String -> AST.Type
typed "int" = integer
typed "double" = double
typed s = error $ "Unknown type" ++ s

parseType :: (String, String) -> (AST.Type, AST.Name)
parseType (t, x) = (typed t, AST.Name x)

toSig :: [(String, String)] -> [(AST.Type, AST.Name)]
toSig = map parseType

codegenTop :: A.TopLevelDecl -> LLVM ()
codegenTop (A.FuncDecl tn args body) = do
  define t name fnargs bls
    where
      (t, name) = parseType tn
      fnargs = toSig args
      bls = createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM args $ \(t1, a) -> do
          let ty = typed t1
          var <- alloca ty
          store ty var (local ty (AST.Name a))
          assign a var
        traverse (\x -> cgen x >>= ret) body

codegenTop (A.ExternFunc tn args) = do
  external t name fnargs
  where
    fnargs = toSig args
    (t, name) = parseType tn

cgen :: A.Expr -> Codegen AST.Operand
cgen (A.Binding (t, x)) = getvar x >>= load (typed t)
cgen (A.Literal (A.IntegerLiteral n)) = return $ cons $ C.Int 64 n
cgen (A.FuncCall  (A.Binding (t, fn):args)) = do
  largs <- mapM cgen args
  let ty = typed t
  call ty (externf ty (AST.Name fn)) largs


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
