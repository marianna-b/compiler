{-# OPTIONS -Wall #-}
module Parser.Lexer
       ( reservedWords
       , spaceConsumer
       , lexeme
       , symbol
       , parens
       , integer
       , float
       , reservedWord
       , identifier
       , builtInType
       ) where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

reservedWords :: [String]
reservedWords = [ "if"
                , "then"
                , "else"
                , "let"
                , "in"
                , "for"
                , "extern"
                , "def"
                , "string"
                , "int"
                , "double"
                ]

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

float :: Parser Double
float = lexeme L.float

reservedWord :: String -> Parser ()
reservedWord w = string w *> notFollowedBy alphaNumChar *> spaceConsumer

builtInType :: String -> Parser String
builtInType w = string w <* notFollowedBy alphaNumChar <* spaceConsumer

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
