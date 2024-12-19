{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types 
import Parser 
import Text.Megaparsec
import Text.Megaparsec.Error
import System.IO 
import Data.Text

main :: IO ()
main = do 
    putStr "loading markdown..." 
    hFlush stdout 
    pseudoMarkdown <- readFile "example.md"
    case parse (parseListItems <* eof) "" (pack pseudoMarkdown) of 
        Left e  -> putStrLn $ errorBundlePretty e 
        Right m -> putStrLn $ show m 
    
