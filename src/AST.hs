module AST
       (
       ) where

data TopLevelDecl = FuncDecl BindingName ParamsList
                  deriving (Show)

data ASTExpr = Literal LiteralValue
             | Binding BindingName
             | FuncCall ASTExpr ASTExpr
             deriving (Show)

type BindingName = String

type ParamsList = [BindingName]

data LiteralValue = IntegerLiteral Integer
                  deriving (Show)
