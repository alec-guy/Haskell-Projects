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
    putStr "Enter markdown: " 
    hFlush stdout 
    pseudoMarkdown <- getLine 
    case parse parseHeading "" (pack pseudoMarkdown) of 
        Left e  -> putStrLn $ errorBundlePretty e 
        Right m -> putStrLn $ show m 
    
