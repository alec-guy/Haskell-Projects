module Types where 

import Data.Text 
import Data.Void (Void)
import Text.Megaparsec 

type Parser = Parsec Void Text 

data Document = MKDOWN [MarkDown] deriving (Show, Eq)

data MarkDown = MarkDown 
              { heading :: [Heading] 
              , paragraph ::[Paragraph] 
              , blockquote :: [BlockQuote]
              , list :: [List]
              , image     ::     [Image] 
              , codeBlock :: [Code]
              , emphasis :: [Emphasis]
              , breakk :: [Break]
              } deriving (Show, Eq)

data Heading = Heading1 Text
             | Heading2 Text
             | Heading3 Text
             | Heading4 Text
             | Heading5 Text
             | Heading6 Text
             deriving (Show, Eq)

newtype Paragraph = OneParagraph [Subparagraph] deriving (Show, Eq)
data Subparagraph = Subparagraph 
                  { t :: Text 
                  , maybeImage :: [Image]  
                  , maybeEmphasis :: [Emphasis]
                  } deriving (Show,Eq)


data Emphasis = Bold Text 
              | Italic Text 
              | BoldAndItalic Text 
              deriving (Show, Eq)

data BlockQuote = BlockQuote Text 
                | NestedBlockQuote Text 
                deriving (Show, Eq)

data List = Ol [ListItem] (Maybe [MarkDown])
          | Ul [ListItem] (Maybe [MarkDown])
          deriving (Show, Eq)

data ListItem = OrderedList (Int, Text)
              | UnorderedList (Char, Text)
              deriving (Show, Eq)
data Image = Image Text Path  deriving (Show, Eq)

newtype Path = Path Text deriving (Show, Eq)

data Code = Code 
          { indentation :: Text 
          , code :: Text 
          } 
          deriving (Show, Eq)

data Break = Break deriving (Show, Eq)