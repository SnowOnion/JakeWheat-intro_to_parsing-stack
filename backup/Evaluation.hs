module Evaluation where

-- import Control.Monad(guard)

data Expr = Lit Double -- 字面量（literal）
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Sqrt Expr -- 平方根
    deriving (Show, Eq) -- 可以按需求 derive 其他的类型类。

-- eval :: Show a => Expr -> Either a Double
eval :: Expr -> Either String Double
eval (Lit x) = return x
eval (Add e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 + v2)
eval (Sub e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 - v2)
eval (Mul e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 * v2)
eval (Div e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      -- guard (v2 /= 0)
                      if v2 /= 0 then return (v1 / v2) else Left "DDZ"
eval (Sqrt e) = do v <- eval e
                   if v >= 0 then return (sqrt v) else Left "Complex"

-- fail 并不能用上 Left
-- guard 好像也不对
