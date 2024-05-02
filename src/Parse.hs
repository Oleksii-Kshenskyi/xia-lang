module Parse (
    Parser,
    runParser,
    Expression(..),
    BinaryOpType(..),
    UnaryOpType(..),
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
    parseExpr,
) where

import Data.Char as C
import Data.Text as T
import Prelude as P
import qualified Data.List as L

newtype Parser a = Parser { runParser :: T.Text -> [(a, T.Text)] }

instance Functor Parser where
     fmap f (Parser p) = Parser $ \input -> [(f x, rest) | (x, rest) <- p input]

instance Applicative Parser where
    pure x = Parser $ \input -> [(x, input)]
    (Parser pf) <*> (Parser px) = Parser $ \input ->
        [(f x, rest2) | (f, rest1) <- pf input, (x, rest2) <- px rest1]

instance Monad Parser where
    return = pure  -- If not using GHC 8.0 or later, you may need to define `return`
    (Parser p) >>= f = Parser $ \input -> L.concatMap (\(x, rest) -> runParser (f x) rest) (p input)

data BinaryOpType =
    Plus | Minus | Multiply | Divide
    deriving Show

data UnaryOpType =
    UPlus
    | UMinus
    deriving Show

data Expression =
    IntLiteral Int
    | UnaryExpression UnaryOpType Expression
    | BinaryExpression Expression BinaryOpType Expression
    deriving Show

isChar :: Char -> Bool
isChar _ = True
charP :: Parser Char
charP = satisfy isChar
char :: Char -> Parser Char
char c = satisfy (== c)

myIsJust :: Maybe a -> Bool
myIsJust m = case m of
    Just _ -> True
    Nothing -> False

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \input -> case T.uncons input of
    Just (c, cs) | p c -> [(c, cs)]
    _ -> []

-- choice operator
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \input -> case runParser p1 input of
    [] -> runParser p2 input
    success -> success

-- whitespace :: Parser ()
-- whitespace (c, cs) = satisfy isSpace cs

digit :: Parser Int
digit = Parser $ \input -> case T.uncons input of
    Just (c, cs) | C.isDigit c -> [(C.digitToInt c, cs)]
    _ -> []

many, many1 :: Parser a -> Parser [a]
many p = many1 p <|> pure []
many1 p = do
    first <- p
    rest <- many p
    pure $ first:rest

whitespace :: Parser ()
whitespace = Parser $ \input -> case T.uncons input of
    Just (c, cs) | isSpace c -> [((), cs)]
    _ -> []

whitespaces :: Parser ()
whitespaces = Parser $ \input -> case runParser (many whitespace) input of
    [(_, rest)] -> [((), rest)]
    _ -> []

-- [1, 2, 3]
-- acc = 0 * 10 + 1 = 1
-- acc = 1 * 10 + 2 = 12
-- acc = 12 * 10 + 3 = 123
number :: Parser Int
number = Parser $ runParser $ L.foldl (\acc x -> acc * 10 + x) 0 <$> many1 digit

charToUnaryOp :: Char -> Maybe UnaryOpType
charToUnaryOp c = case c of
    '+' -> Just UPlus
    '-' -> Just UMinus
    _ -> Nothing

charToBinaryOp :: Char -> Maybe BinaryOpType
charToBinaryOp c = case c of
    '+' -> Just Plus
    '-' -> Just Minus
    '*' -> Just Multiply
    '/' -> Just Divide
    _ -> Nothing

binaryOp :: Parser BinaryOpType
binaryOp = Parser $ \input -> case T.uncons input of
    Just (c, cs) -> case charToBinaryOp c of
        Just bop -> [(bop, cs)]
        Nothing -> []
    _ -> []

unaryOp :: Parser UnaryOpType
unaryOp = Parser $ \input -> case T.uncons input of
    Just (c, cs) -> case charToUnaryOp c of
        Just uop -> [(uop, cs)]
        Nothing -> []
    _ -> []

unaryExpr :: Parser Expression
unaryExpr = do
    whitespaces
    opType <- unaryOp
    whitespaces
    op1 <- number
    whitespaces
    pure $ UnaryExpression opType (IntLiteral op1)

binaryExpr :: Parser Expression
binaryExpr = do
    whitespaces
    op1 <- number
    whitespaces
    opType <- binaryOp
    whitespaces
    op2 <- number
    whitespaces
    pure $ BinaryExpression (IntLiteral op1) opType (IntLiteral op2)

literalExpr :: Parser Expression
literalExpr = do
    whitespaces
    op1 <- number
    whitespaces
    pure $ IntLiteral op1

parseExpr :: Parser Expression
parseExpr = parenthesizedExpr <|> binaryExpr <|> unaryExpr <|> literalExpr

parenthesizedExpr :: Parser Expression
parenthesizedExpr = do
    whitespaces *> char '(' *> whitespaces
    expr <- parseExpr
    whitespaces <* char ')' <* whitespaces
    pure expr