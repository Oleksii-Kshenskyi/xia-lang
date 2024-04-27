module Parse (
    Parser,
    BinaryExpression,
    BinaryOpType,
    digit,
    many, many1, (<|>),
    number,
    whitespaces,
    binaryOp,
    binaryExpr,
    charToBinaryOp,
    satisfy,
    myIsJust,
    charP,
    evalExpr,
) where

import Data.Char as C
import Data.Text as T
import Prelude as P
import Data.List as L

type Parser a = T.Text -> [(a, T.Text)]

data BinaryOpType =
    Plus | Minus | Multiply | Divide
    deriving Show

data BinaryExpression = BinaryExpression Int BinaryOpType Int
    deriving Show

isChar :: Char -> Bool
isChar _ = True
charP :: Parser Char
charP = satisfy isChar

pureP :: a -> Parser a
pureP x input = [(x, input)]

myIsJust :: Maybe a -> Bool
myIsJust m = case m of
    Just _ -> True
    Nothing -> False

satisfy :: (Char -> Bool) -> Parser Char
satisfy p input = case T.uncons input of
    Just (c, cs) | p c -> [(c, cs)]
    _ -> []

-- choice operator
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = \input -> case p1 input of
    [] -> p2 input
    success -> success

-- whitespace :: Parser ()
-- whitespace (c, cs) = satisfy isSpace cs

digit :: Parser Int
digit input = case T.uncons input of
    Just (c, cs) | C.isDigit c -> [(C.digitToInt c, cs)]
    _ -> []

many :: Parser a -> Parser [a]
many p = many1 p <|> pureP []

many1 :: Parser a -> Parser [a]
many1 p input = case p input of
    [] -> []
    toParse -> P.concatMap continueParsing toParse
  where
    continueParsing (x, rest) = case many p rest of
        [] -> [([x], rest)]
        parsed -> [(x : xs, moreRest) | (xs, moreRest) <- parsed]

whitespace :: Parser ()
whitespace input = case T.uncons input of
    Just (c, cs) | isSpace c -> [((), cs)]
    _ -> []

whitespaces :: Parser ()
whitespaces input = case many whitespace input of
    [(_, rest)] -> [((), rest)]
    _ -> []

-- [1, 2, 3]
-- acc = 0 * 10 + 1 = 1
-- acc = 1 * 10 + 2 = 12
-- acc = 12 * 10 + 3 = 123
number :: Parser Int
number input = case many1 digit input of
    [(digits, rest)] -> [(P.foldl (\acc x -> acc * 10 + x) 0 digits, rest)]
    _ -> []

charToBinaryOp :: Char -> Maybe BinaryOpType
charToBinaryOp c = case c of
    '+' -> Just Plus
    '-' -> Just Minus
    '*' -> Just Multiply
    '/' -> Just Divide
    _ -> Nothing

binaryOp :: Parser BinaryOpType
binaryOp input = case T.uncons input of
    Just (c, cs) -> case charToBinaryOp c of
        Just bop -> [(bop, cs)]
        Nothing -> []
    _ -> []

binaryExpr :: Parser BinaryExpression
binaryExpr input = do
    (_, rest) <- whitespaces input
    (op1, rest2) <- number rest
    (_, rest3) <- whitespaces rest2
    (opType, rest4) <- binaryOp rest3
    (_, rest5) <- whitespaces rest4
    (op2, rest6) <- number rest5
    (_, rest7) <- whitespaces rest6
    pureP (BinaryExpression op1 opType op2) rest7

evalExpr :: BinaryExpression -> Int
evalExpr (BinaryExpression op1 opType op2) = case opType of
        Plus -> op1 + op2
        Minus -> op1 - op2
        Multiply -> op1 * op2
        Divide -> op1 `div` op2