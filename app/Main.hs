module Main where

-- import Ch2
-- import Ch3
import ExprParser1
import Eval
import FunctionsAndTypesForParsing
import DoubleLiteral

main :: IO ()
main = undefined


-- > ast = genericExprToExpr <$> parseWithEof genericExpr "10+(98 -5*  4)"
-- > eval <$> ast
-- Right (Just 88.0)
-- eval <$> genericExprToExpr <$> parseWithEof genericExpr "10+(98 -5*  4)"
