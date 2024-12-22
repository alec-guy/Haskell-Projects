module Parser where 

import Types
import Text.Megaparsec 
import Text.Megaparsec.Char 
import Control.Monad.Combinators.Expr 
import Data.Void (Void)
import Control.Monad (void)



type ArgParser = Parsec Void String 

parens :: ArgParser a -> ArgParser a
parens = between (string "(") (string ")")

parseVar :: ArgParser Proposition
parseVar = ((\c -> Var c True) <$> upperChar) <?> "var"

expression :: ArgParser Proposition 
expression = makeExprParser parseTerm table <?> "expression"
 
parseTerm :: ArgParser Proposition 
parseTerm = parens expression <|> parseVar <?> "term"

parsePremise :: ArgParser Proposition 
parsePremise = do 
    exp' <- expression 
    void newline 
    return $ exp'

parseConclusion :: ArgParser Proposition
parseConclusion = do 
    void $ choice $ string <$> ["%", "⊨", "∴"]
    conc      <- expression
    return conc 



parseArgument :: ArgParser Argument 
parseArgument = do 
    (premises1 , conclusoin1) <- manyTill_ parsePremise parseConclusion
    return $ Argument premises1 conclusoin1

table :: [[Operator ArgParser Proposition]]
table = [ [ Prefix  (Not <$ choice (string <$>  ["~"]))
          ]
        , [ InfixL  (And <$ choice (string <$>  ["&&"]))
          , InfixL  (If <$ choice (string <$>  ["->"])) 
          ]
        , [ InfixL  (Iff <$ choice (string <$>  ["<->"])) 
          , InfixL  (Or <$ choice (string <$>  ["||"]))
          ] 
        ]
