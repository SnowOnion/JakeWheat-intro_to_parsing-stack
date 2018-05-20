module Ch3 where
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
-- -- import Text.Parsec.Token(lexeme) -- not alike
--
-- num :: Parser Integer
-- num = do
--     n <- many1 digit
--     return (read n)
--
-- var :: Parser String
-- var = do
--     fc <- firstChar
--     rest <- many nonFirstChar
--     return (fc:rest)
--   where
--     firstChar = satisfy (\a -> isLetter a || a == '_')
--     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')
--
-- newtype Parentheses = Parentheses Integer
--                    deriving (Eq,Show)
--
-- -- 期望的 parsing 结果
-- parensExamples :: [(String, Parentheses)]
-- parensExamples = [("(1)", Parentheses 1) ,("(17)", Parentheses 17)]
--
-- parens :: Parser Parentheses
-- parens = do
--    -- char '('
--    char '('
--    e <- many1 digit
--    -- char ')'
--    char ')'
--    return (Parentheses (read e))
--
--
--
-- data SingleAdd = SingleAdd Integer Integer
--                 deriving (Eq,Show)
--
-- singleAddExamples :: [(String, SingleAdd)]
-- singleAddExamples = [("1+2", SingleAdd 1 2)
--                    ,("101+202", SingleAdd 101 202)]
--
--
-- add :: Parser SingleAdd
-- add = do
--    e0 <- many1 digit
--    char '+'
--    e1 <- many1 digit
--    return (SingleAdd (read e0) (read e1))
--
--
-- whitespace :: Parser ()
-- whitespace = void $ many $ oneOf " \n\t"
--
--
-- parensW :: Parser Parentheses
-- parensW = do
--     whitespace
--     char '('
--     whitespace
--     e <- many1 digit
--     whitespace
--     char ')'
--     whitespace
--     return (Parentheses (read e))
--
-- -- 原教程没有
-- addW :: Parser SingleAdd
-- addW = do
--     whitespace
--     e0 <- many1 digit
--     whitespace
--     char '+'
--     whitespace
--     e1 <- many1 digit
--     whitespace
--     return (SingleAdd (read e0) (read e1))
--
-- lexeme :: Parser a -> Parser a
-- lexeme p = do
--            x <- p
--            whitespace
--            return x
--
-- addW' :: Parser SingleAdd
-- addW' = do
--     whitespace
--     e0 <- lexeme $ many1 digit
--     lexeme $ char '+'
--     e1 <- lexeme $ many1 digit
--     return (SingleAdd (read e0) (read e1))
--
--
-- parseWithWhitespace :: Parser a -> String -> Either ParseError a
-- parseWithWhitespace p = parseWithEof wrapper
--   where
--     wrapper = do
--         whitespace
--         p
--
-- parensL :: Parser Parentheses
-- parensL = do
--     lexeme $ char '('
--     e <- lexeme $ many1 digit
--     lexeme $ char ')'
--     return (Parentheses (read e))
--
-- parseWithWhitespace' :: Parser a -> String -> Either ParseError a
-- parseWithWhitespace' p = parseWithEof (whitespace >> p)
--
-- -- 想得到的抽象语法树
-- data SimpleExpr = Num Integer
--                 | Var String
--                 | Add SimpleExpr SimpleExpr
--                 | Parens SimpleExpr
--                   deriving (Eq,Show)
-- e1 = " (x  + 1)+2+  ((x)) "
--
-- simpleExprExamples :: [(String,SimpleExpr)]
-- simpleExprExamples =
--     [("a", Var "a")
--     ,("1", Num 1)
--     ,("2 + 3", Add (Num 2) (Num 3))
--     ,("(42)", Parens (Num 42))]
--
-- -- component parsers
-- numE :: Parser SimpleExpr
-- numE = do
--     n <- lexeme $ many1 digit
--     return $ Num $ read n
--
-- varE :: Parser SimpleExpr
-- varE = lexeme $ do
--     fc <- firstChar
--     rest <- many nonFirstChar
--     return $ Var (fc:rest)
--   where
--     firstChar = satisfy (\a -> isLetter a || a == '_')
--     nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')
--
--
-- -- -- Here is an alternative, with the call to lexeme in a different place, but gives effectively the same function.
-- -- -- 真对吗？
-- -- varE' :: Parser SimpleExpr
-- -- varE' = do
-- --     fc <- firstChar
-- --     rest <- lexeme $ many nonFirstChar
-- --     return $ Var (fc:rest)
-- --   where
-- --     firstChar = satisfy (\a -> isLetter a || a == '_')
-- --         nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_')
--
-- parensE :: Parser SimpleExpr
-- parensE = do
--     lexeme $ char '('
--     e <- lexeme $ many1 digit
--     lexeme $ char ')'
--     return $ Parens $ Num $ read e
--
-- -- we can reuse the numE parser like this:
-- parensE' :: Parser SimpleExpr
-- parensE' = do
--     lexeme $ char '('
--     e <- numE
--     lexeme $ char ')'
--     return $ Parens e
--
-- addE :: Parser SimpleExpr
-- addE = do
--     e0 <- numE
--     lexeme $ char '+'
--     e1 <- numE
--     return $ Add e0 e1
--
-- numOrVar :: Parser SimpleExpr
-- numOrVar = numE <|> varE
--
-- -- choice 只是对 (<|>) 的包装。根据可读性来选
-- -- foldr 实现的 http://hackage.haskell.org/package/parsec-3.1.13.0/docs/src/Text.Parsec.Combinator.html#choice
-- numOrVar' :: Parser SimpleExpr
-- numOrVar' = choice [numE,varE]
--
-- -- 1. 这个优先级是有问题的，加法永远不生效
-- -- 2. addE and parensE don’t parse general expressions as the components,
-- -- but just numE.
-- simpleExpr :: Parser SimpleExpr
-- simpleExpr = numE <|> varE <|> addE <|> parensE
--
--
-- -- 解答1，Let’s try and rearrange the order:
-- simpleExpr1 :: Parser SimpleExpr
-- simpleExpr1 = addE <|> numE <|> varE <|> parensE
--
-- -- 这样  parseWithWhitespace simpleExpr1 "12" 又会失败！！！
--
-- -- try 函数！实现回溯。竟然通过函数应用就实现了“控制流的改变”
-- -- try :: Parser a -> Parser a
-- simpleExpr2 :: Parser SimpleExpr
-- simpleExpr2 = try addE <|> numE <|> varE <|> parensE
--
-- -- When this is used with (<|>), it means that if the first parser fails, it will undo the consumed input and carry on with the next option, instead of failing completely. This works even if the try is nested deeply within the first parser given to (<|>).
-- -- try 有坏处downsides（啥？）
--
-- -- 解答2
-- parensE3 :: Parser SimpleExpr
-- parensE3 = do
--     lexeme $ char '('
--     e <- simpleExpr3
--     lexeme $ char ')'
--     return $ Parens e
--
-- addE3 :: Parser SimpleExpr
-- addE3 = do
--     e0 <- simpleExpr3
--     lexeme $ char '+'
--     e1 <- simpleExpr3
--     return $ Add e0 e1
--
-- simpleExpr3 :: Parser SimpleExpr
-- simpleExpr3 = try addE3 <|> numE <|> varE <|> parensE3
--
-- -- 这会无限循环。因为有左递归！
-- -- simpleExpr3 and addE3 will keep calling each other
--
-- -- Let’s try without add. 暂时去掉加法。
-- parensE4 :: Parser SimpleExpr
-- parensE4 = do
--     lexeme $ char '('
--     e <- simpleExpr4
--     lexeme $ char ')'
--     return $ Parens e
--
-- simpleExpr4 :: Parser SimpleExpr
-- simpleExpr4 = numE <|> varE <|> parensE4
--
-- -- 可用。
-- -- 得把 add 加回来。实际上是消除左递归的策略：
-- -- A -> A x
-- -- 变成
-- -- A -> B
-- -- B -> A x
--
-- -- 写两个 parser combinator，或者叫 parser combinator。
-- parensEN :: Parser SimpleExpr -> Parser SimpleExpr
-- parensEN simpleExprImpl = do
--     lexeme $ char '('
--     e <- simpleExprImpl
--     lexeme $ char ')'
--     return $ Parens e
--
-- term :: Parser SimpleExpr -> Parser SimpleExpr
-- term simpleExprImpl = numE <|> varE <|> parensEN simpleExprImpl
--
-- term5 :: Parser SimpleExpr
-- term5 = term term5
--
-- addE5 :: Parser SimpleExpr
-- addE5 = do
--     e0 <- term5
--     lexeme $ char '+'
--     e1 <- term5
--     return $ Add e0 e1
--
-- simpleExpr5 :: Parser SimpleExpr
-- simpleExpr5 = try addE5 <|> term5
--
-- -- 能处理单个加法了！但是多个还不行
-- -- > parseWithLeftOver simpleExpr5 "1+a"
-- -- Right (Add (Num 1) (Var "a"),"")
-- -- > parseWithLeftOver simpleExpr5 "1+a+b"
-- -- Right (Add (Num 1) (Var "a"),"+b")
--
--
-- -- add 和 simpleExpr 里的后一个 term 变成 simpleExpr
-- term6 :: Parser SimpleExpr
-- term6 = term simpleExpr6
--
-- addE6 :: Parser SimpleExpr
-- addE6 = do
--     e0 <- term6
--     lexeme $ char '+'
--     e1 <- simpleExpr6
--     return $ Add e0 e1
--
-- simpleExpr6 :: Parser SimpleExpr
-- simpleExpr6 = try addE6 <|> term6
--
-- -- > parseWithLeftOver simpleExpr6 "a + b + c"
-- -- Right (Add (Var "a") (Add (Var "b") (Var "c")),"")
--
-- -- 这是右结合的。
-- -- 如果想左结合呢？
--
-- term7 :: Parser SimpleExpr
-- term7 = term simpleExpr7
--
-- simpleExpr7 :: Parser SimpleExpr
-- simpleExpr7 = do
--     -- first parse a term
--     e <- term7
--     -- then see if it is followed by an '+ expr' suffix
--     maybeAddSuffix e
--   where
--     -- 去解析加法表达式里剩下的部分
--     addSuffix :: SimpleExpr -> Parser SimpleExpr
--     addSuffix e0 = do
--         lexeme $ char '+'
--         e1 <- term7
--         maybeAddSuffix (Add e0 e1)
--     -- this is the wrapper for addSuffix, which adapts it so that if
--     -- addSuffix fails, it returns just the original expression
--     maybeAddSuffix :: SimpleExpr -> Parser SimpleExpr
--     maybeAddSuffix e = addSuffix e <|> return e
--
-- -- > parseWithLeftOver simpleExpr7 "a + b + c"
-- -- Right (Add (Add (Var "a") (Var "b")) (Var "c"),"")
--
-- -- There is a combinator function in Parsec we can use which abstracts this sort of pattern, chainl1.
--
-- simpleExpr8 :: Parser SimpleExpr
-- simpleExpr8 = chainl1 term8 op
--   where
--     op = do
--         lexeme $ char '+'
--         return Add
--     term8 = term simpleExpr8
--
-- -- 用 chainl1 实现的
-- -- chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
-- -- chainl1 term op = ...
-- -- op 只消费那个 + 号。
--
-- -- Add :: SimpleExpr -> SimpleExpr -> SimpleExpr