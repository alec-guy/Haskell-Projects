{-# LANGUAGE OverloadedStrings #-}

module Evaluator where 

import Types
import Data.List (nub, splitAt)
import Control.Monad (replicateM)
import Data.Maybe (fromJust)

---------------------------------------
evalProp :: Proposition -> Bool 
evalProp prop = 
    case prop of 
     (Var _ b)           -> b 
     (Not prop0)         -> not (evalProp prop0)
     (And prop0 prop0')  -> (&&) (evalProp prop0)  (evalProp prop0' )
     (Or prop0 prop0')   -> (||) (evalProp prop0 ) (evalProp prop0') 
     (If prop0 prop0')   -> not ((&&) (evalProp prop0) (not $ evalProp prop0')) 
     (Iff prop0 prop0')  -> (evalProp prop0) == (evalProp prop0') 
     (Nand prop0 prop0') -> not (evalProp prop0 || evalProp prop0')
     (Xor prop0 prop0')  -> evalProp prop0 /= evalProp prop0'
     (Nor prop0 prop0')  -> (not $ evalProp prop0) && (not $ evalProp prop0')
 

checkArgumentSoundness :: Argument -> Soundness 
checkArgumentSoundness argument = 
    if (Valid == (checkArgumentValidity argument)) && (hasTruePremises (premises argument))
    then Sound 
    else Unsound 
-----------------------------
hasTruePremises :: [Proposition] -> Bool 
hasTruePremises propositions = all (== True) (evalProp <$> propositions)

--------------------------------------
checkArgumentValidity :: Argument -> Validity 
checkArgumentValidity argument = 
    if Invalid `elem` (evalArgument argument) then Invalid else Valid  

evalArgument :: Argument -> [Validity]
evalArgument argument = do 
    let assignments   = genAssignments argument 
        assignedPremises = [(((flip assignProp) assignment) <$> (premises argument), assignProp (conclusion argument) assignment) 
                           | assignment <- assignments
                           ]
    premises1Conclusion <- assignedPremises 
    case Nothing `elem` (fst premises1Conclusion) of 
            True  -> [] 
            False -> do let premises1 = fromJust <$> (fst premises1Conclusion)
                            conclusion1 = fromJust $ snd premises1Conclusion
                        case (all (== True) (evalProp <$> premises1)) of 
                            True ->  if not $ evalProp conclusion1 then return $ Invalid else return $ Valid 
                            False -> return $ Valid 


getCellContents :: Argument -> [([Bool], Bool)]
getCellContents argument = do 
    let assignments = genAssignments argument 
        assignedPremises = [(((flip assignProp) assignment) <$> (premises argument), assignProp (conclusion argument) assignment) 
                           | assignment <- assignments
                           ]
    premises1Conclusion <- assignedPremises 
    case Nothing `elem` (fst premises1Conclusion) of 
            True  -> [] 
            False -> return $ (evalProp <$> fromJust <$> (fst premises1Conclusion), evalProp $ fromJust $ snd premises1Conclusion)
fromBool :: Bool -> String
fromBool b = if b then "1" else "0"
----------------------------------------------
assignProp :: Proposition -> [(Char,Bool)] -> Maybe Proposition
assignProp (Var c b) assignment = do 
            bool <- lookup c assignment 
            return $ Var c bool 
assignProp (Not prop0) assignment         = do 
         p1 <- (assignProp prop0 assignment)
         return $ Not p1 
assignProp (And prop0 prop1) assignment  = do
         p1 <- (assignProp prop0 assignment) 
         p2 <- (assignProp prop1 assignment)
         return $ And p1 p2 
assignProp (If prop0 prop1) assignment   = do 
        p1 <- (assignProp prop0 assignment) 
        p2 <- (assignProp prop1 assignment )
        return $ If p1 p2 
assignProp (Iff prop0 prop1) assignment  = do 
        p1 <- (assignProp prop0 assignment) 
        p2 <- (assignProp prop1 assignment)
        return $ Iff p1 p2 
assignProp (Or prop0 prop1) assignment   = do 
        p1 <- (assignProp prop0 assignment) 
        p2 <- (assignProp prop1 assignment)
        return $ Or p1 p2 
assignProp (Xor prop0 prop1) assignment  = do 
        p1 <- (assignProp prop0 assignment) 
        p2 <- (assignProp prop1 assignment)
        return $ Xor p1 p2 
assignProp (Nor prop0 prop1) assignment  = do 
        p1 <- (assignProp prop0 assignment) 
        p2 <- (assignProp prop1 assignment)
        return $ Nor p1 p2 
assignProp (Nand prop0 prop1) assignment = do 
        p1 <- (assignProp prop0 assignment) 
        p2 <- (assignProp prop1 assignment)
        return $ Nand p1 p2 

genAssignments :: Argument -> [[(Char, Bool)]]
genAssignments arg = do 
       let vars = (nub $ concat $ getVars <$> premises arg) :: String
           lengthVars = length vars 
       (zip vars) `fmap` (replicateM lengthVars [True,False])
getVars :: Proposition  -> String 
getVars prop = 
    case prop of 
        (Var c _)         -> [c]
        (Not prop)        -> getVars prop 
        (And prop prop')  -> (getVars prop) ++ (getVars prop')
        (Or prop prop')   -> (getVars prop) ++ (getVars prop')
        (If prop prop')   -> (getVars prop) ++ (getVars prop') 
        (Iff prop prop')  -> (getVars prop) ++ (getVars prop') 
        (Nor prop prop')  -> (getVars prop) ++ (getVars prop') 
        (Nand prop prop') -> (getVars prop) ++ (getVars prop') 
        (Xor prop prop')  -> (getVars prop) ++ (getVars prop')

----------------------------------------------------------

mkPropLogic :: Argument -> PropLogic 
mkPropLogic argument = PropLogic 
                     { validity    = checkArgumentValidity argument 
                     , cellContent = fmap (\(l,b) -> (concat (fromBool <$> l), fromBool b)) (getCellContents argument)
                     }

