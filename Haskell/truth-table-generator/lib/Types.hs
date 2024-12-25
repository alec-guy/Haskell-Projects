{-# LANGUAGE DeriveGeneric #-}
module Types where 

import Data.Aeson 
import GHC.Generics

data Proposition = Var Char  
                 | And  Proposition Proposition 
                 | Or   Proposition Proposition
                 | Iff  Proposition Proposition
                 | If   Proposition Proposition 
                 | Xor  Proposition Proposition
                 | Nor  Proposition Proposition 
                 | Nand Proposition Proposition 
                 | Not  Proposition 
                 deriving (Show, Eq)

data Argument = Argument 
              { premises   :: [Proposition]
              , conclusion :: Proposition 
              } deriving (Eq, Show)
   
data Validity = Valid | Invalid deriving (Show, Eq, Generic)
              
data Soundness = Sound | Unsound deriving (Show, Eq)

data PropLogic = PropLogic 
               {validity       :: Maybe Validity 
               ,cellContent    :: [(String, String)]
               ,vars           :: [String]
               ,varAssignments :: [String]
               } deriving (Show, Eq, Generic)

data PropLogicReq = PropLogicReq 
                  { argToParse :: String 
                  } deriving (Show, Eq, Generic)
    
instance FromJSON PropLogicReq
instance ToJSON Validity where 
instance ToJSON PropLogic where 
