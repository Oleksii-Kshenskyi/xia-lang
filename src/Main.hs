{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text as T

import Parse
import Eval

-- Goal is to parse "let var = (3 + 5) * 7" for now
-- #TODO: parsing single binary op with two numbers, now parse
--       an expression with multiple binary ops.
-- #TODO: parse unary ops.
-- #TODO: parse parenthesized expressions.
testExpr :: Text
testExpr = T.pack "10*99 - 3"

extractExpr :: [(a, b)] -> Maybe a
extractExpr [(x, _)] = Just x
extractExpr _ = Nothing

main :: IO ()
main = do
    print $ runParser (many digit) $ T.pack "KEKW"
    print $ runParser (many digit) $ T.pack "1kek"
    print $ runParser (many digit) $ T.pack "1230"
    print $ runParser (many digit) $ T.pack "123kek"
    print $ runParser (many digit) $ T.pack "123"
    print $ T.pack "==================="
    print $ runParser number $ T.pack ""
    print $ runParser number $ T.pack "KEKW"
    print $ runParser number $ T.pack "1KEK"
    print $ runParser number $ T.pack "666KEK"
    print $ runParser number $ T.pack "KEK666"
    print $ runParser whitespaces $ T.pack "666"
    print $ runParser whitespaces $ T.pack " 666"
    print $ runParser whitespaces $ T.pack "\t666"
    print $ runParser whitespaces $ T.pack "\n666"
    print $ runParser whitespaces $ T.pack "\r666"
    print $ runParser whitespaces $ T.pack "      KEKW "
    print $ runParser whitespaces >> runParser number $ T.pack "  77  "
    print $ runParser binaryOp $ T.pack "-cc"
    print $ runParser (many digit) $ T.pack "6b"
    print $ runParser (many $ satisfy (myIsJust . charToBinaryOp)) $ T.pack "++kes"
    print $ evalExpr <$> extractExpr (runParser parseExpr testExpr)