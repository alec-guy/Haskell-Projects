{-# LANGUAGE DeriveGeneric #-}

module Types where 
import GHC.Generics 
import Data.Aeson

data Command = TodayWeather Zipcode
             | Humidity Zipcode
             | Temp Zipcode
             | Time Zipcode
             | WeekendForecast Zipcode
             deriving (Show, Eq)

newtype Zipcode = Zipcode String  deriving (Show, Eq)

-- {"zip":"78521","name":"Brownsville","lat":25.9221,"lon":-97.4612,"country":"US"}

data LatLonCity = LatLonCity 
                { zip     :: String 
                , name    :: String 
                , lat     :: Float 
                , lon     :: Float
                , country :: String 
                }
                deriving (Show, Eq, Generic)
instance FromJSON LatLonCity where 
