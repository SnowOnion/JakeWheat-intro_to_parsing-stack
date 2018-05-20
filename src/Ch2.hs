module Ch2 where

import Text.Parsec.String (Parser)
-- import Text.Parsec.String.Char (anyChar)
import Text.Parsec.String.Char
import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Data.Char
import Text.Parsec.String.Combinator (many1)

-- 可以直接放 lhs，哇哦…… stack 和 atom 插件都认。
