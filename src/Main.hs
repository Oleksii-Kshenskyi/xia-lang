{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text as T
import Parse as P

main :: IO ()
main = do
    print $ P.many P.digit $ T.pack "KEKW"
    print $ P.many P.digit $ T.pack "1kek"
    print $ P.many P.digit $ T.pack "1230"
    print $ P.many P.digit $ T.pack "123kek"
    print $ P.many P.digit $ T.pack "123"
    print $ T.pack "==================="
    print $ P.number $ T.pack ""
    print $ P.number $ T.pack "KEKW"
    print $ P.number $ T.pack "1KEK"
    print $ P.number $ T.pack "666KEK"
    print $ P.number $ T.pack "KEK666"
