module AST
       ( Module (..)
       , TopLevelDecl (..)
       , Expr (..)
       , TypedBindingName (..)
       , BindingName (..)
       , ParamsList (..)
       , LiteralValue (..)
       ) where

type Module = [TopLevelDecl]

data TopLevelDecl = FuncDecl TypedBindingName ParamsList [Expr]
                  | ExternFunc TypedBindingName ParamsList
                  deriving (Show)

data Expr = Literal LiteralValue
          | Binding BindingName
          | FuncCall [Expr]
          deriving (Show)

type TypedBindingName = (String, String)
type BindingName = String

type ParamsList = [TypedBindingName]

data LiteralValue = IntegerLiteral Integer
                  deriving (Show)
