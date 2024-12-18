module Types where 

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.NonEmpty as CNE
import Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.List.NonEmpty (toList)

data Item    = Phone 
             | Wallet 
             | Keys 
             | Glasses 
             | Shoes 
             | Item String deriving (Show, Eq)

newtype Location = Location String deriving (Show, Eq)

newtype Area = Area [(Item, Location)] deriving (Show, Eq)

newtype Sorted = Sorted [Area] deriving (Show, Eq)

equalLocations :: (Item, Location) -> (Item, Location) -> Bool 
equalLocations (_, l1) (_, l2) = l1 == l2
---------
-- Types parser 
type Parser = Parsec Void String

spaceParser :: Parser () 
spaceParser = 
   L.space hspace1 empty empty


parseItem :: Parser Item
parseItem = do 
    choice [ Wallet  <$ (spaceParser *> string' "wallet")
           , Keys    <$ (spaceParser *> string' "keys")
           , Glasses <$ (spaceParser *> string' "glasses")
           , Shoes   <$ (spaceParser *> string' "shoes")
           , Phone   <$ (spaceParser *> string' "phone")
           , label "Custuom Item" (Item   <$> toList <$> (spaceParser *> (CNE.some anySingle)))
           ]
parseLocation :: Parser Location
parseLocation = label "Custuom Item" (Location <$> toList <$> (spaceParser *> (CNE.some anySingle)))

parsePair :: Parser (Item, Location)
parsePair = do 
    item <- parseItem 
    loc  <- parseLocation 
    pure $ (item, loc)

