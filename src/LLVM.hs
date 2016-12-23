{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LLVM where

import Control.Monad.State
import Data.Map

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

getTypes :: [Definition] -> Map Name Type
getTypes [] = empty
getTypes (GlobalDefinition g:xs) = insert n t $ getTypes xs
  where n = name g
        t = returnType g
getTypes (_:xs) = getTypes xs

define ::  Type -> Name -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty n argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name = n
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  Type -> Name -> [(Type, Name)] -> LLVM ()
external retty n argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name = n
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }
