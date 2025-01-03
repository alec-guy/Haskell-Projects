{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types 
import Evaluator 
import Parser 
import Web.Scotty 
import Control.Exception
import Text.Megaparsec
import Text.Megaparsec.Error
import Data.Text 
import Data.Text.Lazy
import Network.Wai.Middleware.Static


main :: IO ()
main = do 
    htmlRoot <- fromStrict <$> Data.Text.pack <$> readFile "frontend/index.html"
    scotty 3001 $ do
       middleware (staticPolicy (addBase "frontend"))
       post "/PropLogic" $ do 
         jsond <- Web.Scotty.catch (jsonData :: (ActionM PropLogicReq)) $ \e -> do 
                   liftIO $ putStrLn $ show (e :: SomeException)
                   return $ PropLogicReq ""
         let e = parse parseEitherArgOrExpression "" (argToParse jsond) 
         liftIO $ putStrLn "Got /PropLogicReq"
         liftIO $ putStrLn $ show jsond
         case e of 
          Left  _   -> return ()
          Right earg -> do 
                let proplogic = evaluateArgOrProp earg
                json proplogic 
                liftIO $ putStrLn "Sent proplogic"
                liftIO $ putStrLn $ show proplogic
       get "/" $ do  
         html htmlRoot 
         liftIO $ putStrLn "Served Root"

     
    
   
