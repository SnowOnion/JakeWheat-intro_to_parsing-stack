module Ch2 where

import Text.Parsec.String (Parser)
import Text.Parsec.String.Char (anyChar)
import Text.Parsec.String.Char
import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
import Data.Char
import Text.Parsec.String.Combinator (many1)

-- 开篇的一坨 import 就有不存在的……
-- 好吧！是教程带了个 wrapper……
-- 可以直接放 lhs，哇哦…… stack 和 atom 插件都认。
