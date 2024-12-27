module Types where 

data Command = TodayWeather Zipcode
             | Humidity Zipcode
             | Temp Zipcode
             | Time Zipcode
             | WeekendForecast Zipcode
             deriving (Show, Eq)

newtype Zipcode = Zipcode String  deriving (Show, Eq)
