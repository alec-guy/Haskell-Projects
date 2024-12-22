module Main where

import Types 
import Evaluator 
import Parser 
import Web.Scotty 
import Control.Exception
import Text.Megaparsec
import Text.Megaparsec.Error

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
    {-
    get "/PropLogic" $ do 
        jsond <- catch (jsonData :: (ActionM PropLogicReq)) \e -> 
                   liftIO $ putStrLn $ show (e :: SomeException)
                   return $ PropLogicReq ""
    -}
    argument <- readFile "argument.txt"
    putStrLn $ "Hello world"
    case parse parseArgument "" argument of 
        Left  e   -> putStrLn $ errorBundlePretty e  
        Right arg -> putStrLn $ show $ mkPropLogic arg
     
    
   
