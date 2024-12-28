
{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text
import Data.Aeson 
import GHC.Generics
import Data.Time.Clock  
import Data.Time.Calendar.OrdinalDate 
import Data.Set
import Text.Megaparsec 
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer 
import Text.Megaparsec.Error
import Data.Void (Void)
import Control.Monad.Combinators
import Data.List 
import Data.Char (isLetter, isDigit,isAlphaNum)
import System.IO (hFlush , stdout)


----Types 
newtype Username = Username Text deriving (Show, Eq, Generic)
newtype Password = Password Text deriving (Show, Eq, Generic)
instance FromJSON Username where 
instance FromJSON Password where 
data User = User 
          {username       :: Username
          ,password       :: Password  
          ,posts          :: [BlogPost]
          } deriving (Show, Eq, Generic)

data BlogPost = BlogPost 
              {blogPost    :: Text 
              ,timeCreated :: (UTCTime, Day)
              }
              deriving (Show , Eq, Generic)
instance FromJSON BlogPost where 
instance FromJSON User where 


------------------
--Parser 
type Parser = Parsec Void Text 

spaceParser :: Parser ()
spaceParser = 
    Text.Megaparsec.Char.Lexer.space hspace1 Control.Monad.Combinators.empty Control.Monad.Combinators.empty
lexemeParser :: Parser a -> Parser a 
lexemeParser = 
    Text.Megaparsec.Char.Lexer.lexeme spaceParser

parseUsername :: Parser Username 
parseUsername = do 
    chrs <- many (alphaNumChar <|> (char '-') <|> (char '_'))
    case (Data.List.length chrs <= 20 ) of 
        True -> 
            case (Data.List.length (Data.List.filter isAlphaNum chrs)) > (fromIntegral $ round ((fromIntegral $ Data.List.length chrs) / 2 )) of 
                True -> return $ Username $ pack $ chrs 
                False -> parseError $ FancyError 1 (Data.Set.singleton $ ErrorFail "username must have more than half numbers or letters than '-' or '_' symbols")
        False -> parseError $ FancyError 1 (Data.Set.singleton $ ErrorFail "username must be less than or equal to 20 characters")
parsePassword :: Parser Password 
parsePassword = undefined
------------

main :: IO ()
main = do 
    putStrLn "Hello, Haskell!"
    putStr "Enter username: "
    hFlush stdout
    username <- getLine 
    let maybeU = parse parseUsername "" (pack username) 
    case maybeU of 
        Left e  -> putStrLn $ errorBundlePretty e 
        Right u -> putStrLn $ show u

