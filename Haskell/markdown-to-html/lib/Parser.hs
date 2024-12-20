{-# LANGUAGE OverloadedStrings #-}
module Parser where 

import Data.Text 
import Text.Megaparsec 
import Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char 
import Control.Monad.Combinators 
import Types 
import Data.Int (Int8 (..))
import Control.Monad (void)
import Data.Either (isLeft, rights, fromRight)
import Data.Char (digitToInt)
import Data.List (intersperse, concat)



spaceParser :: Parser ()
spaceParser = L.space hspace1 Text.Megaparsec.empty Text.Megaparsec.empty 

parseWhiteSpaceAfterLexeme :: Parser a -> Parser a 
parseWhiteSpaceAfterLexeme = L.lexeme spaceParser

parseWords :: Parser Text 
parseWords = pack <$> (many (alphaNumChar <|> (char ' ')))

parseWord :: Parser Char 
parseWord = (alphaNumChar <|> (char ' '))

parsePseudoMarkDown :: Parser MarkDown 
parsePseudoMarkDown = do 
   h  <- try $ observing (parseHeading) 
   p  <-  try $ observing (parseParagraph )
   bq <-  try $ observing (parseBlockQuote )
   l  <-  try $ observing (parseListItems )
   i  <-  try $ observing (parseImage )
   c  <-  try $ observing (parseCodeBlock )
   e  <-  try $ observing (parseEmphasis  )
   return MarkDown 
          {heading    = if isLeft h then  [] else rights  [h]
          ,paragraph  = if isLeft p then  [] else rights  [p]
          ,blockquote = if isLeft bq then [] else rights [bq]
          ,list       = if isLeft l then  [] else fromRight [] l 
          ,image      = if isLeft i then  [] else rights  [i]
          ,codeBlock  = if isLeft c then  [] else fromRight [] c
          ,emphasis   = if isLeft e then  [] else rights  [e]
          }
   


parseParagraph :: Parser Paragraph 
parseParagraph = do 
    subs <- many parseSubparagraph
    return $ OneParagraph subs 

parseSubparagraph :: Parser Subparagraph 
parseSubparagraph = do 
    emph <- try $ observing (parseEmphasis)
    img  <- try $ observing (parseImage)
    text <- parseWords <* newline 
    return $ Subparagraph
             {t = text 
             ,maybeEmphasis = if  isLeft emph then [] else  rights [emph]
             ,maybeImage    = if  isLeft img then  [] else  rights [img]
             }

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
              numChar <- manyTill (alphaNumChar <|> (char ' ')) (void newline <|> eof) -- for now, but I think you can embed other symbols
              return $ heading $ (pack hash) `append` (pack numChar)

--------------------------------------

parseEmphasis :: Parser Emphasis 
parseEmphasis = choice [try parseBold 
                       ,try parseItalic
                       ,try parseBoldAndItalic
                       ]
parseBold :: Parser Emphasis 
parseBold = do 
  opening <- choice [string "**"
                    ,string "__"
                    ]
  text    <- parseWords
  closing <- choice [string "**" 
                    ,string "__"
                    ]
  case opening == closing of 
    True ->  return $ Bold $ text
    _    ->  error "bold error"

parseItalic :: Parser Emphasis 
parseItalic = do 
  opening <- choice [string "*" 
                    ,string "_"
                    ]
  text    <- parseWords 
  closing <- choice [ string "*"
                    , string "_"
                    ]
  case opening == closing of 
    True  -> return $ Italic $ text
    False -> error "bold error"

parseBoldAndItalic :: Parser Emphasis 
parseBoldAndItalic = do 
  opening <- choice [(,) (1 :: Int8) <$> string "***"
                    ,(,) (2 :: Int8)  <$> string "___"
                    ,(,) (3 :: Int8) <$> string "__*"
                    ,(,) (4 :: Int8) <$> string "**_"
                    ]                   -- This logic also doesn't work
  text <- parseWords  
  closing <- choice [(,) (1 :: Int8) <$> string "***"
                    ,(,) (2 :: Int8) <$> string "___"
                    ,(,) (3 :: Int8) <$> string "*__"
                    ,(,) (4 :: Int8) <$> string "_**"
                    ]
  case (fst closing) == (fst opening) of 
   True -> return $ BoldAndItalic $ text
   False -> error "Bold and Italic error" 

---------------------
     
parseBlockQuote :: Parser BlockQuote 
parseBlockQuote = do 
    symbol      <- eitherP (char '>') (string ">>")
    markdown    <- many parsePseudoMarkDown
    case symbol of 
        Left  s  -> return (BlockQuote markdown)
        Right ss -> return (NestedBlockQuote markdown) 
     
---------------------------------------
parseListItems :: Parser [List]
parseListItems = do 
    items <- many $ do 
              item <- parseListItem 
              e <- eitherP (string "    " <|> (string "\t")) (return [])
              case e of 
                Left  _ ->  do 
                             marks <- many parsePseudoMarkDown 
                             return (item, Just marks)
                Right _    ->  return (item, Nothing) 
    return $ f <$> items 
    where f :: (ListItem, Maybe [MarkDown]) -> List  
          f ((OrderedList tup), m)  = Ol [OrderedList tup] m 
          f ((UnorderedList tup), m) = Ul [UnorderedList tup] m

parseListItem :: Parser ListItem 
parseListItem = do 
    let parseulChar = choice [ char '-'
                             , char '*' 
                             , char '+' 
                             , char '-'
                             ]
    numberOrSymbol <- eitherP (digitChar <* (char '.')) parseulChar
    case numberOrSymbol of 
        Left  num  -> do 
                       itemText <- parseWords <* newline
                       return $ OrderedList (digitToInt num,itemText)
        Right sym  ->  do 
                        itemText <- parseWords <* newline
                        return $ UnorderedList (sym,itemText)
{-
filter_ :: [a] -> (a -> Bool) -> ([a], [a])
filter_ l f = do 
    let is = filter f l 
        isnt = filter (not f) l
    (is, isnt)
-}
----------------------------------------------

-- parseWords = pack <$> (many (alphaNumChar <|> (char ' '))) 
-- To Parse code, we need non alphanumeric characters. such as ':' in haskell , (::) 
-- so we must for now, use printChar until a better solution is found. 

parseCode :: Parser Code 
parseCode = do 
   let parsecode = pack <$> (many (printChar <|> (char ' ')))
   indent    <- many $ (string "    " <|> (string "\t"))
   code      <- parsecode <* newline 
   return $ Code (Data.Text.concat indent) code

parseCodeBlock :: Parser [Code]
parseCodeBlock = many parseCode 
----------------------
parseImage :: Parser Image 
parseImage = do 
    void $ (char '!') 
    imageWords <- (between (char '[') (char ']') (many printChar)) 
    path <- between (char '(') (char ')') parsePath
    return $ Image (pack $ imageWords) path 

parsePath :: Parser Path
parsePath = do
    let noParen = noneOf ['(', ')']
    text <- sepEndBy (many noParen) (char '/')
    return $ Path $ pack $ Data.List.concat $ (Data.List.intersperse "/" text)
