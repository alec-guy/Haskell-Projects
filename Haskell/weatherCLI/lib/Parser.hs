module Parser where 

import Text.Megaparsec 
import Text.Megaparsec.Char 
import Text.Megaparsec.Char.Lexer
import Data.Void (Void)
import Types
import Control.Monad.Combinators 

type Parser = Parsec Void String 

spaceParser :: Parser ()
spaceParser =  
     Text.Megaparsec.Char.Lexer.space hspace1 empty empty

lexemeParser ::  Parser a -> Parser a
lexemeParser = lexeme spaceParser

parseCommand :: Parser Command 
parseCommand = do 
    choice [try parseTodayWeather
           ,try parseHumidity 
           ,try parseTemp 
           ,try parseTime 
           ,try parseWeekendForecast 
           ]
    where parseTodayWeather :: (Parser Command)
          parseTodayWeather = do 
            zipCode <- spaceParser *> ((lexemeParser $ string "today-weather") *> (lexemeParser $ parseZipCode))
            return $ TodayWeather zipCode
          parseHumidity :: Parser Command 
          parseHumidity = do 
            zipCode <- spaceParser *> ((lexemeParser $ string "today-humidity ") *> (lexemeParser $ parseZipCode))
            return $ Humidity zipCode
          parseTemp :: Parser Command 
          parseTemp = do 
            zipCode <- spaceParser *> ((lexemeParser $ string "today-temp ") *> (lexemeParser $ parseZipCode))
            return $ Temp zipCode
          parseTime :: Parser Command 
          parseTime = do 
            zipCode <- spaceParser *> ((lexemeParser $ string "time ") *> (lexemeParser $ parseZipCode))
            return $ Time zipCode 
          parseWeekendForecast :: Parser Command 
          parseWeekendForecast = do 
            zipCode <- spaceParser *> ((lexemeParser $ string "weekend-forecast ") *> (lexemeParser $ parseZipCode))
            return $ WeekendForecast zipCode
parseZipCode :: Parser Zipcode
parseZipCode = (lexemeParser (Zipcode <$> (count 5 digitChar))) <* eof


    
    

