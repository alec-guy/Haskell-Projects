{-# LANGUAGE OverloadedStrings #-}

module Main where
import Text.Megaparsec
import Parser 
import Types
import Text.Megaparsec.Error
import Network.HTTP.Simple 
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Data.ByteString.Char8 as C8
import System.IO (hFlush, stdout)
import Control.Exception (catch, SomeException(..))
import Data.Aeson


getZipCode :: Command -> Zipcode 
getZipCode command = 
    case command of 
        (TodayWeather zipcode)    -> zipcode 
        (WeekendForecast zipcode) -> zipcode
        (Humidity zipcode)        -> zipcode 
        (Temp zipcode)            -> zipcode 
        (Time zipcode)            -> zipcode 

hostnameAPI :: Request 
hostnameAPI = undefined 

main :: IO ()
main = do 
    Prelude.putStrLn "Hello, Haskell!"
    Prelude.putStr $ "Enter command: "
    hFlush stdout
    command <- Prelude.getLine 
    case parse parseCommand "" command of 
        Left err  -> Prelude.putStrLn $ errorBundlePretty err
        Right com -> do 
            bodyR <- let bs = httpBS hostnameAPI 
                     in  bs
            return ()


            

