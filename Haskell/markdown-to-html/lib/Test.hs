{-# LANGUAGE OverloadedStrings #-}
module Test where 

import Data.Text 
import Text.Megaparsec
import Text.Megaparsec.Error 
import Test.Tasty 
import Test.Tasty.HUnit
import Parser 
import Types 


blockQuote :: Text 
blockQuote = "> Hello world"



paragraphTest :: TestTree 
paragraphTest = testCase "paragraphTest" $ do 
    let output = parse parseParagraph "" ""
    case output of 
        Left e -> putStrLn $ errorBundlePretty e 
        Right p -> putStrLn $ show p