module Main where

import Types 
import Evaluator 

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
    putStrLn $ show $ checkArgumentValidity modusPonens
    putStrLn $ show $ checkArgumentValidity invalidArgument
    putStrLn $ show $ checkArgumentSoundness modusPonens 
    
   
