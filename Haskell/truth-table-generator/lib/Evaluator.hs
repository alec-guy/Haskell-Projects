
module Evaluator where 

import Types
import Data.List (nub, splitAt, null, last, singleton)
import Control.Monad (replicateM)
import Data.Maybe (fromJust)
import Data.Tuple (swap)


type Assignment = (Char, Bool)
newtype PossibleWorld = PossibleWorld Bool deriving (Show, Eq)
toPossibleWorld :: Bool -> PossibleWorld 
toPossibleWorld = PossibleWorld 

modusponens = Argument [If (Var 'P') (Var 'Q'), Var 'P'] (Var 'Q')

mkPropLogic :: Argument -> PropLogic
mkPropLogic arg = 
            let vars = getVars $ (premises arg) ++ [conclusion arg]
            in  PropLogic 
                {validity        = computeValidity arg 
                ,cellContent     = getCellContent arg
                , vars           =  singleton <$> vars 
                , varAssignments =  ((<$>) (\b -> if b then '1' else '0')) <$> (replicateM (length vars) [True,False])
                }
showMaybeBool :: Maybe Bool ->  String 
showMaybeBool Nothing = ""
showMaybeBool (Just b) = if b then "1" else "0"

getCellContent :: Argument -> [(String,String)]
getCellContent arg = 
       let prem              = premises arg 
           conc              = conclusion arg 
           assignments       = getAssignments arg 
           cellContentProto  = [( assignment, (((flip evalPropAt) assignment) <$> prem)) | assignment <- assignments]
           cellContents      = swap <$> [(showMaybeBool (evalPropAt conc assignment)
                                         , concat $ showMaybeBool <$> premiseEvals
                                         ) 
                                        | (assignment, premiseEvals) <- cellContentProto 
                                        ]

       in cellContents
       

computeValidity :: Argument -> Validity 
computeValidity arg = 
     let assignments = getAssignments arg 
         possibleWorlds = (getPossibleWorlds (premises arg) assignments)
         truePossibleWorlds1 = filter truePossibleWorlds possibleWorlds 
     in 
     case truePossibleWorlds1 of 
        [] -> Valid 
        l  -> case getPossibleWorlds [conclusion arg] (getAssignmentsFromPossibleWorlds truePossibleWorlds1) of 
               [] -> Valid 
               l2 -> if all truePossibleWorlds l2 then Valid else Invalid



getAssignmentsFromPossibleWorlds :: [([Assignment], PossibleWorld)] -> [[Assignment]]
getAssignmentsFromPossibleWorlds l = 
    case l of 
        [] -> [] 
        ((assignment, _) : t) -> assignment : (getAssignmentsFromPossibleWorlds t)

truePossibleWorlds :: ([Assignment], PossibleWorld) -> Bool
truePossibleWorlds (_,(PossibleWorld False)) = False 
truePossibleWorlds _                     = True 

fromMaybeBool :: Maybe Bool -> String 
fromMaybeBool Nothing = ""
fromMaybeBool (Just b) = if b then "1" else "0"
---------------------------------------------

getPossibleWorlds :: [Proposition] -> [[Assignment]] -> [([Assignment], PossibleWorld)]
getPossibleWorlds prem assignments = (\assignment -> (,) assignment (evalPremises prem assignment)) <$> assignments

evalPremises :: [Proposition] -> [Assignment] -> PossibleWorld   
evalPremises prem assignment = toPossibleWorld (foldr (\p acc ->  (evalPropAt p assignment) `maybeAnd` acc) True  prem)

maybeAnd :: Maybe Bool -> Bool -> Bool 
maybeAnd mb b = 
    case mb of 
        Nothing  -> False && b
        (Just b') -> b' && b
        
-- Ugly ahh function 
evalPropAt :: Proposition -> [Assignment] -> Maybe Bool 
evalPropAt (Var c) assignments = lookup c assignments
evalPropAt (Not p) assignments           = not <$> (evalPropAt p assignments)
evalPropAt (And p p2) assignments        = do 
    b  <- evalPropAt p assignments 
    b2 <- evalPropAt p2 assignments
    return $ b && b2 
evalPropAt (Or p p2) assignments      = do 
    b  <- evalPropAt p assignments
    b2 <- evalPropAt p2 assignments
    return $ b || b2 
evalPropAt (If p p2) assignments       = do 
    let ifThen b b2 = not (b && (not b2))
    b  <- evalPropAt p assignments
    b2 <- evalPropAt p2 assignments
    return $ b `ifThen` b2 
evalPropAt (Iff p p2) assignments       = do 
    let iff b b2 = b == b2
    b  <- evalPropAt p assignments
    b2 <- evalPropAt p2 assignments
    return $ b `iff` b2 
evalPropAt (Xor p p2) assignments       = do 
    let xor b b2 = b /= b2
    b  <- evalPropAt p assignments
    b2 <- evalPropAt p2 assignments
    return $ b `xor` b2 
evalPropAt (Nor p p2) assignments      = do 
    let nor b b2 = (not b) && (not b2)
    b  <- evalPropAt p assignments
    b2 <- evalPropAt p2 assignments
    return $ b `nor` b2 
evalPropAt (Nand p p2) assignments      = do 
    let nand b b2 = not (b && b2)
    b  <- evalPropAt p assignments
    b2 <- evalPropAt p2 assignments
    return $ b `nand` b2 
------------------

getAssignments :: Argument -> [[Assignment]]
getAssignments argument = do 
      let vars           = getVars (premises argument)
      (zip vars) <$> (replicateM (length vars) [True, False])

getVars :: [Proposition] -> String 
getVars props =  nub $ concat $ getVars' <$> props 
        where getVars' :: Proposition -> String
              getVars' prop = 
                case prop of 
                 (Var c) -> [c]
                 (Not p) -> getVars' p
                 (And p p') -> (getVars' p) ++ (getVars' p')
                 (Or p p')  -> (getVars' p) ++ (getVars' p')
                 (If p p')  -> (getVars' p) ++ (getVars' p')
                 (Iff p p') -> (getVars' p) ++ (getVars' p')
                 (Xor p p') -> (getVars' p) ++ (getVars' p')
                 (Nand p p') -> (getVars' p) ++ (getVars' p')
                 (Nor p p') ->  (getVars' p) ++ (getVars' p')
                              
