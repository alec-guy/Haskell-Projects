{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types 
import Parser 
import Text.Megaparsec
import Text.Megaparsec.Error
import System.IO 
import Data.Text
import Text.Megaparsec.Debug





main :: IO ()
main = do 
    hFlush stdout 
    pseudoMarkdown <- readFile "example.md"
    putStrLn "Markdown loaded"
    case parse (parseMarkDownDocument) "" (pack pseudoMarkdown) of 
        Left e  -> putStrLn $ errorBundlePretty e 
        Right m -> putStrLn $ show m 
