{-# OPTIONS -Wall #-}
module Parser.Parser where

import Text.Megaparsec
import Text.Megaparsec.String
--import Debug.Trace(trace)

import AST ( Module
           , TopLevelDecl (..)
           , Expr (..)
           , BindingName
           , TypedBindingName
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
        params = many bindingDecl
        body = some $ expr <* L.symbol ";"

externFuncDecl :: Parser TopLevelDecl
externFuncDecl = ExternFunc <$> (L.reservedWord "extern" *> name) <*> params
  where name = bindingDecl
        params = many bindingDecl

expr :: Parser Expr
expr = try (FuncCall <$> funcCall) <|> ifthen <|> for <|> letins <|>
       finalExpr

finalExpr :: Parser Expr
finalExpr = L.parens expr <|>
            Literal <$> literal <|>
            Binding <$> binding

ifthen :: Parser Expr
ifthen = do
  L.reservedWord "if"
  cond <- expr
  L.reservedWord "then"
  tr <- expr
  L.reservedWord "else"
  fl <- expr
  return $ IF cond tr fl

for :: Parser Expr
for = do
  L.reservedWord "for"
  var <- bindingDecl
  _ <- L.symbol "="
  start <- finalExpr
  _ <- L.symbol ","
  cond <- expr
  _ <- L.symbol ","
  step <- expr
  L.reservedWord "in"
  body <- expr
  return (For var start cond step body) --trace (show $ For var start cond step body) (For var start cond step body)

letins :: Parser Expr
letins = do
  L.reservedWord "let"
  (t, var) <- bindingDecl
  _ <- L.symbol "="
  val <- expr
  L.reservedWord "in"
  body <- expr
  return $ Let (t, var) val body

types :: Parser String
types = L.builtInType "double" <|> L.builtInType "int"

binding :: Parser BindingName
binding = L.identifier

bindingDecl :: Parser TypedBindingName
bindingDecl = (,) <$> (types <* L.symbol ":") <*> L.identifier

literal :: Parser LiteralValue
literal = try (FloatLiteral <$> L.float) <|> (IntegerLiteral <$> L.integer)

funcCall :: Parser [Expr]
funcCall = (:) <$> finalExpr <*> some finalExpr
