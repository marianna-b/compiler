module Parser.Parser
       (
       ) where

import Text.Megaparsec
import Text.Megaparsec.String

import AST ( Module (..)
           , TopLevelDecl (..)
           , Expr (..)
           , BindingName (..)
           , ParamsList (..)
           , LiteralValue (..))
import qualified Parser.Lexer as L

sourceFile :: Parser Module
sourceFile = L.spaceConsumer *> topLevelDecls <* eof

topLevelDecls :: Parser [TopLevelDecl]
topLevelDecls = some topLevelDecl

topLevelDecl :: Parser TopLevelDecl
topLevelDecl = topLevelFuncDecl

topLevelFuncDecl :: Parser TopLevelDecl
topLevelFuncDecl = FuncDecl <$> name <*> params <* L.symbol "=" <*> body
  where name = bindingDecl
        params = some binding
        body = expr

expr :: Parser Expr
expr = try (FuncCall <$> funcCall) <|>
       finalExpr

finalExpr :: Parser Expr
finalExpr = L.parens expr <|>
            Literal <$> literal <|>
            Binding <$> binding

binding :: Parser BindingName
binding = L.identifier

bindingDecl :: Parser BindingName
bindingDecl = L.identifier

literal :: Parser LiteralValue
literal = IntegerLiteral <$> L.integer

funcCall :: Parser [Expr]
funcCall = (:) <$> finalExpr <*> some finalExpr
