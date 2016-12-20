module AST
       ( Module (..)
       , TopLevelDecl (..)
       , Expr (..)
       , BindingName (..)
       , ParamsList (..)
       , LiteralValue (..)
       ) where

type Module = [TopLevelDecl]

data TopLevelDecl = FuncDecl BindingName ParamsList Expr
                  deriving (Show)

data Expr = Literal LiteralValue
          | Binding BindingName
          | FuncCall [Expr]
          deriving (Show)

type BindingName = String

type ParamsList = [BindingName]

data LiteralValue = IntegerLiteral Integer
                  deriving (Show)
