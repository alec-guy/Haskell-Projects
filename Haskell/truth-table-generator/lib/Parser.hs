module Parser where 

import Types
import Text.Megaparsec 
import Text.Megaparsec.Char 
import Control.Monad.Combinators.Expr 
import Data.Void (Void)



type ArgParser = Parsec Void String 

parseVar :: ArgParser Proposition
parseVar = ((\c -> Var c True) <$> upperChar) <?> "var"

expression = makeExprParser parseTerm table <?> "expression"