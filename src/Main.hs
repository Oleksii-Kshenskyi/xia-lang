{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text as T
import Parse

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
