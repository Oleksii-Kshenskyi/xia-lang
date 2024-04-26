module Parse (
    Parser,
    digit,
    many,
    number,
) where

import Data.Char as C
import Data.Text as T
import Prelude as P

type Parser a = T.Text -> [(a, T.Text)]

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = \input -> case p1 input of
    [] -> p2 input
    success -> success

digit :: Parser Int
digit input = case T.uncons input of
    Just (c, cs) | C.isDigit c -> [(C.digitToInt c, cs)]
    _ -> []

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p input = case p input of
    [] -> []
    toParse -> P.concatMap continueParsing toParse
  where
    continueParsing (x, rest) = case many p rest of
        [] -> [([x], rest)]
        parsed -> [(x : xs, moreRest) | (xs, moreRest) <- parsed]

-- [1, 2, 3]
-- acc = 0 * 10 + 1 = 1
-- acc = 1 * 10 + 2 = 12
-- acc = 12 * 10 + 3 = 123
number :: Parser Int
number input = case many digit input of
    [(digits, rest)] -> [(P.foldl (\acc x -> acc * 10 + x) 0 digits, rest)]
    _ -> []
