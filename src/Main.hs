{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text as T
import Parse as P

main :: IO ()
main = do
    print $ P.digit $ T.pack "KEKW"
    print $ P.digit $ T.pack "1kek"
