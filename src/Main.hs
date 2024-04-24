{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text as T
import Data.Text.IO as I

testText :: T.Text
testText = "Text wroks!!1"

main :: IO ()
main = do
    I.putStrLn testText
