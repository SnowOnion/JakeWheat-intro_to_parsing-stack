module ExprParser1 where

import Text.Parsec.String (Parser)
import Text.Parsec.String.Combinator (many1, between)
import Text.Parsec.String.Char (letter, char, digit, string, oneOf)
import Control.Applicative ((<$>), (<*>), (<*), (<|>), many, (<$))
import Control.Monad (void)
import qualified Text.Parsec.String.Expr as E
import FunctionsAndTypesForParsing

import ExprDef(Expr(..))
import DoubleLiteral(double)


whitespace :: Parser ()
whitespace = void $ many $ oneOf "\r\n \t"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace -- TODO 不太一样……

integer :: Parser Integer
integer = read <$> lexeme (many1 digit)

identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar

symbol :: String -> Parser String
symbol s = lexeme $ string s

-- 通用表达式，通用在何处呢？BinaryOp 表示二元运算符，PrefixOp 表示。这样可以任意扩展运算符的种类。
-- 比如加入 % ^ < <= > >= ==
-- 甚至赋值号 =
data GenericExpr = Num Double
                 -- | Var String
                 -- | Parens GenericExpr
                 | PrefixOp String GenericExpr
                 | BinaryOp GenericExpr String GenericExpr
                   deriving (Eq,Show)

genericExpr :: Parser GenericExpr
genericExpr = E.buildExpressionParser table term

-- 文档有小例子 http://hackage.haskell.org/package/parsec-3.1.13.0/docs/Text-Parsec-Expr.html
-- 以运算符为元素二维列表。优先级越高的运算符放在越前面；相同优先级的放在一个列表里；
-- 哪些可以搞成相同优先级的？顺序任意都不会有误解的的时候。如1-2+3解释为1+(-2)+3，1/2*3解释为1*(1/2)*3
table :: [[E.Operator GenericExpr]]
table = [
         [prefix "-"],
         [binary "*" E.AssocLeft, binary "/" E.AssocLeft],
         [binary "+" E.AssocLeft, binary "-" E.AssocLeft]
        ]
    where
    binary name assoc = E.Infix (mkBinOp name <$ symbol name) assoc
    mkBinOp nm a b = BinaryOp a nm b
    prefix name = E.Prefix (PrefixOp name <$ symbol name)


term :: Parser GenericExpr
term =
  -- var <|>
  num <|> parens
num :: Parser GenericExpr
num = Num <$> double
-- var :: Parser GenericExpr
-- var = Var <$> identifier
parens :: Parser GenericExpr
parens = between (symbol "(") (symbol ")") genericExpr

-- http://hackage.haskell.org/package/parsec-3.1.13.0/docs/src/Text.Parsec.Token.html#local-6989586621679063917
-- symbol name = lexeme (string name)


-- (BinaryOp (BinaryOp (BinaryOp (Num 0) "*" (Num 1)) "/" (Num 2)) "*" (Num 3))
src =
  BinaryOp
  (BinaryOp
   (BinaryOp (Num 0) "*" (Num 1))
    "/" (Num 2))
   "*" (Num 3)

genericExprToExpr :: GenericExpr -> Expr
genericExprToExpr (Num x) = Lit x
genericExprToExpr (PrefixOp "-" ge) = Sub (Lit 0) (genericExprToExpr ge)
genericExprToExpr (PrefixOp "+" ge) = genericExprToExpr ge
genericExprToExpr (BinaryOp ge1 op ge2) = case op of
  "*" -> Mul (genericExprToExpr ge1) (genericExprToExpr ge2)
  "/" -> Div (genericExprToExpr ge1) (genericExprToExpr ge2)
  "+" -> Add (genericExprToExpr ge1) (genericExprToExpr ge2)
  "-" -> Sub (genericExprToExpr ge1) (genericExprToExpr ge2)
-- genericExprToExpr (Var n) = Var1 n
