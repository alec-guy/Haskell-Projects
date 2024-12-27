module Main where
import Text.Megaparsec
import Parser 
import Types
import Text.Megaparsec.Error
main :: IO ()
main = do 
    putStrLn "Hello, Haskell!"
    putStrLn $ "Enter command: "
    command <- getLine 
    case parse parseCommand "" command of 
        Left err  -> putStrLn $ errorBundlePretty err
        Right com -> putStrLn $ show com
