module MyLib (Triangle(..),Side(..), TA(..)) where 

import Data.Angle 

data TA = TR (Radians Double)
        | TD (Degrees Double)
        deriving (Eq, Show, Ord)

newtype Base   = Base Float  deriving (Eq, Show)
newtype Height = Height Float deriving (Eq, Show)

data Triangle = Triangle 
              { base ::   (Base, Maybe TA)
              , height :: (Maybe Height, Maybe TA)
              , sides :: ((Maybe Float, Maybe TA), (Maybe Float, Maybe TA))
              }
--------------------------------------------
-- Calculating area of a triangle 

areaOfTriangle :: Base -> Height -> Float 
areaOfTriangle (Base b) (Height h) = (1 / 2) * b * h 

getArea :: Triangle -> Maybe Float 
getArea t = 
    case getSides t of 
        (base, Nothing, (Nothing, Nothing)) -> Nothing 
        (base, Just x, (Nothing, Nothing))  -> Just (areaOfTriangle base x)
        


----------------------------------------------
getSides :: Triangle -> (Base,Maybe Height,(Maybe Float, Maybe Float))
getSides t = 
    (fst $ base t, fst $ height t, (fst $ fst $ sides t, fst $ fst $ sides t))

getTAs :: Triangle -> (Maybe TA,Maybe TA,(Maybe TA, Maybe TA))
getTAs t =
    (snd $ base t, snd $ height t, (snd $ snd $ sides t, snd $ snd $ sides t))
--------------------------------------------






