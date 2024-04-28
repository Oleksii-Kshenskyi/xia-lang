module Parse (
    Parser,
    runParser,
    BinaryExpression(..),
    BinaryOpType(..),
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

data BinaryExpression = BinaryExpression Int BinaryOpType Int
    deriving Show

isChar :: Char -> Bool
isChar _ = True
charP :: Parser Char
charP = satisfy isChar

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

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = Parser $ \input -> case runParser p input of
    [] -> []
    toParse -> P.concatMap continueParsing toParse
  where
    continueParsing (x, rest) = case runParser (many p) rest of
        [] -> [([x], rest)]
        parsed -> [(x : xs, moreRest) | (xs, moreRest) <- parsed]

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
number = Parser $ \input -> case runParser (many1 digit) input of
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
binaryOp = Parser $ \input -> case T.uncons input of
    Just (c, cs) -> case charToBinaryOp c of
        Just bop -> [(bop, cs)]
        Nothing -> []
    _ -> []

binaryExpr :: Parser BinaryExpression
binaryExpr = do
    whitespaces
    op1 <- number
    whitespaces
    opType <- binaryOp
    whitespaces
    op2 <- number
    whitespaces
    pure $ BinaryExpression op1 opType op2