module Main where

import Web.scotty 
import Triangle 
import Data.Aeson


main :: IO ()
main = do 
    scotty 3000 $ do 
        get "/"
         html htmlRoot 
        
        
    

