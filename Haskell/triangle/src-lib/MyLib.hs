module MyLib (Triangle(..),TA(..)) where 

import Data.Angle 

data TA = TR (Radians Double)
        | TD (Degrees Double)
        deriving (Eq, Show, Ord)

newtype Base   = Base Float  deriving (Eq, Show)
newtype Height = Height Float deriving (Eq, Show)

data Triangle = Triangle 
              { base ::   (Base)
              , height :: (Maybe Height)
              , sides :: (Maybe Float, Maybe Float)
              , angles :: (Maybe TA, Maybe TA, Maybe TA)
              }
--------------------------------------------
-- Calculating area of a triangle 

areaTriangle :: Base -> Height -> Float 
areaTriangle (Base b) (Height h) = (1 / 2) * b * h 

getArea :: Triangle -> Maybe Float 
getArea t = 
    case getSides t of 
        (base, Nothing, (Nothing, Nothing)) -> Nothing 
        (base, Just h, (Nothing, Nothing))  -> Just (areaTriangle base h)
        _ -> 
             

----------------------------------------------
getSides :: Triangle -> (Base,Maybe Height,(Maybe Float, Maybe Float))
getSides t = 
    (base t, height t, (fst $ sides t, snd $ sides t))

isEqualatiral :: Triangle -> Bool 
isEqualatiral triangle = 
    case getSides triangle of 
        ((Base b), _ , (Just x, Just y)) -> 
            case angles triangle of 
                (Nothing, Nothing, Nothing) -> 
                   (b == x) && (b == y) 
isIsosceles  :: Triangle -> Bool 
isIsosceles triangle = 
    case getSides triangle of 
        ((Base b)_ , (Just x, Just y)) -> 
            (b /= x) && (x == y)
            
isScalene :: Triangle -> Bool 
isScalene triangle = 
    (not $ isEqualatiral triangle) && (not $ isIsosceles triangle)
--------------------------------------------






