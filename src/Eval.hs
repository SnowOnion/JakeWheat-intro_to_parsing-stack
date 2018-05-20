module Eval where

import Control.Monad(guard)
import ExprDef(Expr(..))

-- the new implementation using monad
eval :: Expr -> Maybe Double
-- just pack the data
-- it is the same as `pure` because monad types are always applicative
eval (Lit x) = return x
-- monad is also a functor
-- eval (Sqrt e) = fmap sqrt (eval e)
-- the monad way
eval (Add e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 + v2)
-- can also be more concise
eval (Sub e1 e2) = do v1 <- eval e1; v2 <- eval e2; return (v1 - v2)
eval (Mul e1 e2) = do v1 <- eval e1; v2 <- eval e2; return (v1 * v2)

-- or in a more simpler way
eval (Div e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      guard (v2 /= 0)
                      return (v1 / v2)
