{-# OPTIONS -Wall #-}
module IR where

import qualified LLVM.General.Module as M
import qualified LLVM.General.Context as CTX
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C

import Control.Monad.Except
import Control.Monad.State

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
toSig = map parseType

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

cgen :: A.Expr -> Codegen AST.Operand
cgen (A.Binding x) = getvar x >>= load
cgen (A.Literal (A.IntegerLiteral n)) = return $ cons $ C.Int 64 n
cgen (A.FuncCall  (A.Binding fn:args)) = do
  t <- getFuncType $ AST.Name fn
  largs <- mapM cgen args
  call (externf t (AST.Name fn)) largs
cgen _ = error "Unsupported expression"

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


