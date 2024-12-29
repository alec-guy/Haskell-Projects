
{-# LANGUAGE DeriveGeneric #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import System.IO 
import Data.ByteString
import Data.ByteString.Lazy 


----Types 
newtype Username = Username Text deriving (Show, Eq, Generic)
newtype Password = Password Text deriving (Show, Eq, Generic)

instance ToJSON Username where 
instance ToJSON Password where 
instance FromJSON Username where 
instance FromJSON Password where 

data User = User 
          {username       :: Username
          ,password       :: Password  
          } deriving (Show, Eq, Generic)

data BlogPost = BlogPost 
              {blogPost    :: Text 
              ,timeCreated :: (UTCTime, Day)
              ,blogCreator :: Username
              }
              deriving (Show , Eq, Generic)

instance FromJSON BlogPost where 
instance FromJSON User where 
instance ToJSON User where 
instance ToJSON BlogPost where 

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
                True -> case notSameCharacter chrs of 
                         True ->  return $ Username $ Data.Text.pack $ chrs 
                         False -> parseError $ FancyError 1 (Data.Set.singleton $ ErrorFail "username cannot have the same character")
                False -> parseError $ FancyError 1 (Data.Set.singleton $ ErrorFail "username must have more than half numbers or letters than '-' or '_' symbols")
        False -> parseError $ FancyError 1 (Data.Set.singleton $ ErrorFail "username must be no greater than 20 characters")
parsePassword :: Parser Password 
parsePassword = do 
    chars <- many printChar 
    case (Data.List.length chars <= 20) of 
        True -> case (Data.List.length chars >= 9) of 
                 True  -> case notSameCharacter chars of 
                           True  -> return $ Password $ Data.Text.pack chars
                           False -> parseError $ FancyError 1 (Data.Set.singleton $ ErrorFail "password cannot have the same character")
                 False -> parseError $ FancyError 1 (Data.Set.singleton $ ErrorFail "password must be at least 9 characters long")
        False -> parseError $ FancyError 1 (Data.Set.singleton $ ErrorFail "password must be no greater than 20 characters")

notSameCharacter :: Eq a => [a] -> Bool 
notSameCharacter [] = True 
notSameCharacter (x : xs) = not $ (Data.List.all (== x) (xs))

------------
-- Saving 

saveUser :: User -> IO () 
saveUser (User {username = u, ..}) = do 
    let encoded = Data.ByteString.append (Data.ByteString.toStrict $ encode $ User {username = u, ..}) "\n"
    Data.ByteString.appendFile "users.txt" (encoded)

saveBlogpost :: BlogPost -> IO () 
saveBlogpost (BlogPost {blogPost = b, ..}) = do 
    let encoded = Data.ByteString.append (Data.ByteString.toStrict $ encode $ BlogPost {blogPost = b, ..}) "\n"
    Data.ByteString.appendFile "blogposts.txt" ( encoded)
------------

main :: IO ()
main = do 
    System.IO.putStrLn "Hello, Haskell!"
    System.IO.putStr "Enter Username: "
    hFlush stdout 
    usern <- System.IO.getLine 
    System.IO.putStr "Enter Password: "
    hFlush stdout
    passwd <- System.IO.getLine 
    usernn <- case parse parseUsername "" (Data.Text.pack usern) of 
               Left  err -> error $ errorBundlePretty err 
               Right us  -> return us 
    psswdd   <- case parse parsePassword "" (Data.Text.pack passwd) of 
                 Left  err -> error $ errorBundlePretty err 
                 Right ps  -> return ps
    let currentUser = (User {username = usernn, password = psswdd})
    saveUser currentUser
    System.IO.putStr "Enter blogpost: "
    hFlush stdout 
    blogpostt <- System.IO.getLine 
    currenttime <- getCurrentTime
    saveBlogpost (BlogPost {blogPost = Data.Text.pack blogpostt, timeCreated = (currenttime, utctDay currenttime),blogCreator = username currentUser})