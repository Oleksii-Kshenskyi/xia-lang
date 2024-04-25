module Parse (
    Parser,
    digit,
 ) where

import Data.Text as T
import Data.Char as C


type Parser a = T.Text -> [(a, T.Text)]

digit :: Parser Int
digit input = case T.uncons input of
    Just (c, cs) | C.isDigit c -> [(C.digitToInt c, cs)]
    _ -> []