module ExprParser1Bad where
--
-- import Text.Parsec (ParseError)
-- import Text.Parsec.String (Parser)
-- import Text.Parsec.String.Parsec (try)
-- import Text.Parsec.String.Char (oneOf, char, digit, satisfy)
-- import Text.Parsec.String.Combinator (many1, choice, chainl1)
-- import Control.Applicative ((<|>), many)
-- import Control.Monad (void)
-- import Data.Char (isLetter, isDigit)
-- import FunctionsAndTypesForParsing
--
-- import Control.Applicative ((<$>), (<*>), (<*), (<|>), many, (<$))
-- import qualified Text.Parsec.String.Expr as E
--
-- data Expr = Lit Int -- Double
--   | Add Expr Expr
--   -- | Sub Expr Expr
--   | Mul Expr Expr
--   -- | Div Expr Expr
--   | Parens Expr -- 先带着括号
--     deriving (Show, Eq)
--
-- expr1 :: Parser Expr
-- expr1 = chainl1 term1 (opMul <|> opAdd)
--   where
--     opAdd = do
--         lexeme $ char '+'
--         return Add
--     opMul = do
--         lexeme $ char '*'
--         return Mul
--     term1 = term expr1
--
-- term :: Parser Expr -> Parser Expr
-- term exprImpl = numE <|> parensEN exprImpl
--
-- numE :: Parser Expr
-- numE = do
--     n <- lexeme $ many1 digit
--     return $ Lit $ read n
--
-- parensEN :: Parser Expr -> Parser Expr
-- parensEN exprImpl = do
--     lexeme $ char '('
--     e <- exprImpl
--     lexeme $ char ')'
--     return $ Parens e
--
-- -- data SimpleExpr = Num Integer
-- --                 | Var String
-- --                 | Add SimpleExpr SimpleExpr
-- --                 | Parens SimpleExpr
-- --                   deriving (Eq,Show)
--
-- -- simpleExpr8 :: Parser SimpleExpr
-- -- simpleExpr8 = chainl1 term8 op
-- --   where
-- --     op = do
-- --         lexeme $ char '+'
-- --         return Add
-- --     term8 = term simpleExpr8
--
-- -- term :: Parser SimpleExpr -> Parser SimpleExpr
-- -- term simpleExprImpl = numE <|> varE <|> parensEN simpleExprImpl
--
-- -- numE :: Parser SimpleExpr
-- -- numE = do
-- --     n <- lexeme $ many1 digit
-- --     return $ Num $ read n
--
-- -- varE :: Parser SimpleExpr
-- -- varE = lexeme $ do
-- --     fc <- firstChar
-- --     rest <- many nonFirstChar
-- --     return $ Var (fc:rest)
-- --   where
-- --     firstChar = satisfy (\a -> isLetter a || a == '_')
-- --     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')
--
-- -- parensEN :: Parser SimpleExpr -> Parser SimpleExpr
-- -- parensEN simpleExprImpl = do
-- --     lexeme $ char '('
-- --     e <- simpleExprImpl
-- --     lexeme $ char ')'
-- --     return $ Parens e
--
-- lexeme :: Parser a -> Parser a
-- lexeme p = do
--            x <- p
--            whitespace
--            return x
--
-- whitespace :: Parser ()
-- whitespace = void $ many $ oneOf "\r\n \t"