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

type ApiKey = String 
insertZipGeo :: Zipcode -> ApiKey -> String 
insertZipGeo (Zipcode zipcode) myapikey = 
    "/geo/1.0/zip?zip=" ++ zipcode ++ ",US&appid=" ++ myapikey 

insertLatLon :: LatLonCity -> ApiKey -> String 
insertLatLon latloncitt myapikey = 
    "data/3.0/onecall?lat=" ++ (show $ lat latloncitt) ++ "&lon=" ++ (show $ lon latloncitt)++ "&exclude=current&appid=" ++ myapikey

getZipCode :: Command -> Zipcode 
getZipCode command = 
    case command of 
        (TodayWeather zipcode)    -> zipcode 
        (WeekendForecast zipcode) -> zipcode
        (Humidity zipcode)        -> zipcode 
        (Temp zipcode)            -> zipcode 
        (Time zipcode)            -> zipcode 

main :: IO ()
main = do 
    Prelude.putStrLn "Hello, Haskell!"
    Prelude.putStr $ "Enter api key: "
    hFlush stdout 
    apikey  <- Prelude.getLine 
    Prelude.putStr $ "Enter command: "
    hFlush stdout
    command <- Prelude.getLine 
    case parse parseCommand "" command of 
        Left err  -> Prelude.putStrLn $ errorBundlePretty err
        Right com -> do 
            latloncityR  <-  let bs = httpBS (setRequestPath (C8.pack $ insertZipGeo (getZipCode com) apikey) "http://api.openweathermap.org") 
                             in  bs 
            let maybelatloncity = (decode $ LBS.fromStrict $ getResponseBody latloncityR)  :: (Maybe LatLonCity)
            case maybelatloncity of 
                Nothing -> do 
                    Prelude.putStrLn  "Could not find out your city, nor geolocation necessary to get weather."
                    Prelude.putStrLn  "I am sorry I cannot offer a better error message."
                    Prelude.putStrLn  "Maybe its the API Key, try changing it."
                    Prelude.putStrLn  "Restarting program."
                    main 
                Just latloncity -> do 
                   unknown <- let bs = httpBS (setRequestPath (C8.pack $ insertLatLon latloncity apikey) "http://api.openweathermap.org")
                              in bs 
                   BS.putStr (getResponseBody unknown)
                   hFlush stdout 


            

