module Parser.Parser
       (
         parseFile
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


parseFile :: String -> Either ParseError Module
parseFile = parse sourceFile "<stdin>"

sourceFile :: Parser Module
sourceFile = L.spaceConsumer *> topLevelDecls <* eof

topLevelDecls :: Parser [TopLevelDecl]
topLevelDecls = some topLevelDecl

topLevelDecl :: Parser TopLevelDecl
topLevelDecl = topLevelFuncDecl <|> externFuncDecl

topLevelFuncDecl :: Parser TopLevelDecl
topLevelFuncDecl = FuncDecl <$> (L.reservedWord "def" *> name) <*> params <* L.symbol "=" <*> body
  where name = bindingDecl
        params = many binding
        body = some $ expr <* L.symbol ";"

externFuncDecl :: Parser TopLevelDecl
externFuncDecl = ExternFunc <$> (L.reservedWord "extern" *> name) <*> params
  where name = bindingDecl
        params = many binding

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
