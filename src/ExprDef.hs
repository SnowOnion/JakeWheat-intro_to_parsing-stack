module ExprDef where

data Expr = Lit Double -- Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Var1 String
    deriving (Show, Eq)
