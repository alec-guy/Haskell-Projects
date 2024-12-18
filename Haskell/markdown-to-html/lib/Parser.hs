{-# LANGUAGE OverloadedStrings #-}
module Parser where 

import Data.Text 
import Text.Megaparsec 
import Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char 
import Control.Monad.Combinators 
import Types 
import Data.Int (Int8 (..))
import Data.Void (Void)
import Control.Monad (void)



type Parser = Parsec Void Text 

spaceParser :: Parser ()
spaceParser = L.space hspace1 Text.Megaparsec.empty Text.Megaparsec.empty 

parseWhiteSpaceAfterLexeme :: Parser a -> Parser a 
parseWhiteSpaceAfterLexeme = L.lexeme spaceParser

------------
parseHeading :: Parser Heading 
parseHeading = do 
    choice [try $ parseHeadingNumber 6 
           ,try $ parseHeadingNumber 5 
           ,try $ parseHeadingNumber 4 
           ,try $ parseHeadingNumber 3
           ,try $ parseHeadingNumber 2 
           ,try $ parseHeadingNumber 1
           ]
parseHeadingNumber :: Int8 ->  Parser Heading 
parseHeadingNumber int = 
    case int of 
        1 -> parseH (Heading1) 1
        2 -> parseH (Heading2) 2
        3 -> parseH (Heading3) 3
        4 -> parseH (Heading4) 4
        5 -> parseH (Heading5) 5
        6 -> parseH (Heading6) 6
        _ -> error "heading error"
    where parseH :: (Text -> Heading) -> Int -> Parser Heading 
          parseH heading i = do 
              hash <- parseWhiteSpaceAfterLexeme $ Control.Monad.Combinators.count i (char' '#') 
              numChar <- manyTill (alphaNumChar <|> (char ' ')) newline -- for now, but I think you can embed other symbols
              return $ heading $ (pack hash) `append` (pack numChar)
-----------
parseParagraph :: Parser Paragraph 
parseParagraph = do 
    p <- many alphaNumChar 
    return $ Paragraph $ pack p 

--------------------
parseEmphasis :: Parser Emphasis 
parseEmphasis = choice [parseBold 
                       ,parseItalic
                       ,parseBoldAndItalic
                       ]
parseBold :: Parser Emphasis 
parseBold = do 
  void (string "**" <|> string "__")
  text    <- many alphaNumChar 
  void (string "**" <|> string "__")
  return $ Bold $ pack text 

parseItalic :: Parser Emphasis 
parseItalic = do 
  void (string "*" <|> string "_")
  text    <- many alphaNumChar 
  void (string "*" <|> string "_")
  return $ Italic $ pack text 

parseBoldAndItalic :: Parser Emphasis 
parseBoldAndItalic = do 
  void $ choice [string "***"
                ,string "___"
                ,string "__*"
                ,string "**_"
                ]
  text <- many alphaNumChar 
  void $ choice [string "***"
                ,string "___"
                ,string "*__"
                ,string "_**"
                ]
  return $ BoldAndItalic $ pack text 

---------------------
parsePseudoMarkDown :: Parser [MarkDown]
parsePseudoMarkDown = do 
     markdowns <- many (choice [H <$> parseHeading 
                               ,P <$> parseParagraph
                               ,E <$> parseEmphasis
                               ]  
                       )
     return markdowns