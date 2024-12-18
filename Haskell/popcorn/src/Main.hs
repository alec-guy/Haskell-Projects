{-# LANGUAGE OverloadedStrings #-}

module Main where 

import Web.Scotty 
import Types
import Data.Text as T
import Data.Text.Lazy
import Data.ByteString 

import Control.Exception hiding (catch)

main :: IO ()
main = do 
    stringHTML    <- Prelude.readFile "frontend/index.html"
    poppedPNG     <- Data.ByteString.readFile "images/popcornKernel.png"
    unpoppedPNG   <- Data.ByteString.readFile "images/seed.png"
    halfPoppedPNG <- Data.ByteString.readFile "images/halfPopped.png"
    let textHTML = Data.Text.Lazy.fromStrict $ T.pack stringHTML
    scotty 3000 $ do 
      get "/" $ do 
         html textHTML
      post "/popcorn" $ do 
        popcornNum <- catch (jsonData :: ActionM JsonPopcorn) $ \e -> do 
                         liftIO $ putStrLn $ show (e :: SomeException) 
                         return $ (JsonPopcorn "")
        liftIO $ putStrLn $  show $ popcornNum
        case popcornNum of 
         (JsonPopcorn "") -> return ()
         (JsonPopcorn s)  -> do 
                    let popcornBowl = fromPsuedoList $ Prelude.take (read s) pseudoPopcornBowl
                    json popcornBowl
      get"/popped" $ do 
        liftIO $ putStrLn "popped image request"
        setHeader "Content-Type" "image/png"
        raw (Data.ByteString.fromStrict poppedPNG)
      get "/unpopped" $ do 
        liftIO $ putStrLn "unpopped image request"
        setHeader "Content-Type" "image/png"
        raw (Data.ByteString.fromStrict unpoppedPNG)
      get "/halfPopped" $ do 
        liftIO $ putStrLn "halfPopped image request"
        setHeader "Content-Type" "image/png"
        raw (Data.ByteString.fromStrict halfPoppedPNG)




         