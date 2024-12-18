module Types where 

import Data.Text 
import Data.Void (Void)
import Text.Megaparsec 

type Parser = Parsec Void Text 

data MarkDown = H Heading 
              | P Paragraph
              | B BlockQuote
              | L [List]
              | I Image 
              | C [Code] 
              | E Emphasis
              deriving (Show, Eq)

data Heading = Heading1 Text
             | Heading2 Text
             | Heading3 Text
             | Heading4 Text
             | Heading5 Text
             | Heading6 Text
             deriving (Show, Eq)

data Paragraph = Paragraph Text 
               { p :: Text
               , maybeImage    :: [Maybe Image]
               , maybeEmphasis :: [Maybe Emphasis]
               , line2 :: Maybe Paragraph
               }


data Emphasis = Bold Text 
              | Italic Text 
              | BoldAndItalic Text 
              deriving (Show, Eq)

data BlockQuote = BlockQuote [MarkDown]
                | NestedBlockQuote [MarkDown]
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

