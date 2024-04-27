{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text as T
import Parse

-- Goal is to parse "let var = (3 + 5) * 7" for now
testExpr :: Text
testExpr = T.pack "10 / 10"

leftT :: [(a, b)] -> Maybe a
leftT [(x, _)] = Just x
leftT _ = Nothing

main :: IO ()
main = do
    print $ many digit $ T.pack "KEKW"
    print $ many digit $ T.pack "1kek"
    print $ many digit $ T.pack "1230"
    print $ many digit $ T.pack "123kek"
    print $ many digit $ T.pack "123"
    print $ T.pack "==================="
    print $ number $ T.pack ""
    print $ number $ T.pack "KEKW"
    print $ number $ T.pack "1KEK"
    print $ number $ T.pack "666KEK"
    print $ number $ T.pack "KEK666"
    print $ whitespaces $ T.pack "666"
    print $ whitespaces $ T.pack " 666"
    print $ whitespaces $ T.pack "\t666"
    print $ whitespaces $ T.pack "\n666"
    print $ whitespaces $ T.pack "\r666"
    print $ whitespaces $ T.pack "      KEKW "
    print $ whitespaces >> number $ T.pack "  77  "
    print $ binaryOp $ T.pack "-cc"
    print $ many digit $ T.pack "6b"
    print $ many (satisfy (myIsJust . charToBinaryOp)) $ T.pack "n+kes"
    print $ evalExpr <$> leftT (binaryExpr testExpr)

    -- print $ test $ T.pack ""