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

modusPonens :: Argument 
modusPonens = Argument 
            {premises   = [If (Var 'P' True) (Var 'Q' True)
                          ,Var 'P' True
                          , Var 'L' True
                          ]
            ,conclusion = Var 'Q' True
            }
invalidArgument :: Argument 
invalidArgument = Argument 
                {premises = [If (Var 'P' True) (Var 'Q' True)]
                ,conclusion = Var 'Q' False
                }

main :: IO ()
main = do 
    htmlRoot <- fromStrict <$> Data.Text.pack <$> readFile "frontend/index.html"
    scotty 3000 $ do 
       post "/PropLogic" $ do 
         jsond <- Web.Scotty.catch (jsonData :: (ActionM PropLogicReq)) $ \e -> do 
                   liftIO $ putStrLn $ show (e :: SomeException)
                   return $ PropLogicReq ""
         let e = parse parseArgument "" (argToParse jsond) 
         liftIO $ putStrLn "Got /PropLogicReq"
         liftIO $ putStrLn $ show jsond
         case e of 
          Left  _   -> return ()
          Right arg -> do 
                let proplogic = mkPropLogic arg 
                json proplogic 
                liftIO $ putStrLn "Sent proplogic"
                liftIO $ putStrLn $ show proplogic
       get "/" $ do  
         html htmlRoot 
         liftIO $ putStrLn "Served Root"

     
    
   
