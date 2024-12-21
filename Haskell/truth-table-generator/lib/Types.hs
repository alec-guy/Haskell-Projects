module Types where 

import Data.Aeson 

data Proposition = Var Char Bool 
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
   
data Validity = Valid | Invalid deriving (Show, Eq)
              
data Soundness = Sound | Unsound deriving (Show, Eq)

data PropLogic = PropLogic 
               {validity    :: Validity 
               ,cellContent :: [(String, Char)]
               } deriving (Show, Eq)