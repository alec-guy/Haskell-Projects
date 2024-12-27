{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty 
import Network.Wai.Middleware.Static
import Data.Text 
import Data.Text.Lazy



main :: IO ()
main = do 
    htmlRoot <- fromStrict <$> Data.Text.pack <$> readFile "frontend/index.html"
    scotty 3000 $ do 
        middleware (staticPolicy (addBase "frontend"))
        get "/" $ do
         html htmlRoot 
        
        
    

