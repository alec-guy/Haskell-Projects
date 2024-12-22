module Parser where 

import Types
import Text.Megaparsec 
import Text.Megaparsec.Char  
import Text.Megaparsec.Char.Lexer as L 
import Control.Monad.Combinators.Expr 
import Data.Void (Void)
import Control.Monad (void)



type ArgParser = Parsec Void String 

spaceConsumer :: ArgParser ()
spaceConsumer = L.space hspace1 empty empty 

lexemeP :: ArgParser a -> ArgParser a 
lexemeP = lexeme spaceConsumer

parens :: ArgParser a -> ArgParser a
parens = between (string "(") (string ")")

parseVar :: ArgParser Proposition
parseVar = lexemeP (((\c -> Var c True) <$> upperChar) <?> "var")

expression :: ArgParser Proposition 
expression = lexemeP (makeExprParser parseTerm table <?> "expression")
 
parseTerm :: ArgParser Proposition 
parseTerm = lexemeP (parens expression <|> parseVar <?> "term")

parsePremise :: ArgParser Proposition 
parsePremise = lexemeP $ do 
    exp' <- expression 
    void newline 
    return $ exp'

parseConclusion :: ArgParser Proposition
parseConclusion = lexemeP $ do 
    void $ choice $ (lexemeP . string) <$> ["%", "⊨", "∴"]
    conc      <- expression
    return conc 



parseArgument :: ArgParser Argument 
parseArgument = lexemeP $ do 
    (premises1 , conclusoin1) <- manyTill_ parsePremise parseConclusion
    return $ Argument premises1 conclusoin1

table :: [[Operator ArgParser Proposition]]
table = [ [ Prefix  (Not <$ choice ((lexemeP . string) <$>  ["~"]))
          ]
        , [ InfixL  (And <$ choice ((lexemeP . string) <$>  ["&&"]))
          , InfixL  (If <$ choice ((lexemeP . string) <$>  ["->"])) 
          ]
        , [ InfixL  (Iff <$ choice ((lexemeP . string) <$>  ["<->"])) 
          , InfixL  (Or <$ choice ((lexemeP . string) <$>  ["||"]))
          ] 
        ]
