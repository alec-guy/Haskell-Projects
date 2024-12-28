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


