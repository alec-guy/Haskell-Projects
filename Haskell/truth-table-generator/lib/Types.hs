{-# LANGUAGE DeriveGeneric #-}
module Types where 

import Data.Aeson 
import Data.Text 
import Data.Aeson.KeyMap as K
import Data.Vector as V
import Data.Aeson.Key 
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
               {validity    :: Validity 
               ,cellContent :: [(String, String)]
               } deriving (Show, Eq, Generic)

data PropLogicReq = PropLogicReq 
                  { argToParse :: String 
                  } deriving (Show, Eq, Generic)
    
instance FromJSON PropLogicReq
instance ToJSON Validity where 
instance ToJSON PropLogic where 
