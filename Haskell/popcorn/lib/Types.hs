{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where 
import System.Random 
import Data.Aeson
import Data.Text

data Popcorn = Unpopped 
             | HalfPoped 
             | Popped
             deriving (Show , Eq, Ord)

data Bowl a  = Single a (Bowl a)
             | Air
             deriving (Show , Eq)
newtype JsonPopcorn = JsonPopcorn String deriving (Show, Eq)

instance FromJSON JsonPopcorn where 
    parseJSON (String t) = return $ JsonPopcorn (unpack t)

instance Foldable Bowl where 
    foldr binaryFunc acc folder = 
        case folder of 
         Air           -> acc 
         (Single x xs) -> binaryFunc x (Prelude.foldr binaryFunc acc xs)

instance Functor Bowl where 
    fmap f Air  = Air 
    fmap f bowl = Prelude.foldr (\a acc -> Single (f a) acc) Air bowl 

instance ToJSON (Bowl Popcorn) where 
    toJSON Air     = Null 
    toJSON popcorn =  String $ pack $ toList $ popToChar <$> popcorn

toList :: Bowl a -> [a]
toList Air = [] 
toList (Single x xs) = x : (toList xs)

popToChar :: Popcorn -> Char 
popToChar Unpopped = 'u' 
popToChar HalfPoped = 'h' 
popToChar Popped = 'p' 

-----------------------------------------------

toPopcorn i  
    | i < 2 = Unpopped 
    | i < 6 = HalfPoped 
    | otherwise = Popped 

pseudoPopcornBowl :: [Int]
pseudoPopcornBowl = randomRs (0, 15) (mkStdGen 2024)


fromPsuedoList :: [Int] -> Bowl Popcorn
fromPsuedoList [] = Air 
fromPsuedoList (x : xs) = Single (toPopcorn x) (fromPsuedoList xs)


