module DoubleLiteral where

-- TODO 清理一下
import Text.Parsec.String (Parser)
import Text.Parsec.String.Combinator (many1, between, option, choice)
import Text.Parsec.String.Char (letter, char, digit, string, oneOf)
import Control.Applicative ((<$>), (<*>), (<*),  some, (<$))
import Control.Monad (void)
import qualified Text.Parsec.String.Expr as E
import FunctionsAndTypesForParsing
import Text.Parsec.Prim

-- FL base exponent
data DoubleLiteral = DoubleLiteral Double Integer
  deriving(Show)

-- 正负号等 expr 处理。
doubleLiteral :: Parser DoubleLiteral
doubleLiteral = do
  -- baseL <- many1 digit <|> pure "0" -- 等效写法
  baseL <- option "0" $ many1 digit
  baseR <- option "0" $ do
    char '.'
    option "0" (many1 digit)
  expon <- option "1" $ do
    choice [char 'e', char 'E']
    expSign <- option '+' $ choice [char '+', char '-']
    exponNum <- many1 digit
    return ((if expSign == '+' then "" else "-") ++ exponNum)
  return $ DoubleLiteral
    -- (read baseL * (if sign == '-' then -1 else 1))
    (read $ baseL ++ '.':baseR)
    (read expon)

double :: Parser Double
double = do
  (DoubleLiteral base expon) <- doubleLiteral
  return (base * 10 ^ expon) -- 展示忘加括号
