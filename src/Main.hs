{-# OPTIONS -Wall #-}
module Main where

import Parser(parseFile)
import Codegeneration
import IR

import Control.Monad.Trans
import System.Environment
import System.Console.Haskeline

import qualified LLVM.General.AST as AST


initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseFile source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop md = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- liftIO $ process md input
        case modn of
          Just mdn -> loop mdn
          Nothing -> loop md

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    (fname:_) -> processFile fname >> return ()
