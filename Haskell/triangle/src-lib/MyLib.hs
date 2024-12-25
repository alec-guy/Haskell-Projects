module MyLib (Triangle(..),TA(..), exampleTriangle, getArea, isEqualatiral, isIsosceles, isScalene) where 

import Data.Angle 

data TA = TR (Radians Float)
        | TD (Degrees Float)
        deriving (Eq, Show, Ord)

newtype Base   = Base Float  deriving (Eq, Show)
newtype Height = Height Float deriving (Eq, Show)

data Triangle = Triangle 
              { base ::   (Base)
              , height :: (Maybe Height)
              , sides :: (Maybe Float, Maybe Float)
              , angles :: (Maybe TA, Maybe TA, Maybe TA)
              } deriving (Show, Eq)
--------------------------------------------
-- Calculating area of a triangle 

exampleTriangle :: Triangle 
exampleTriangle = Triangle {base = Base 10, height = Nothing, sides = (Just 11, Just 12), angles = (Nothing,Nothing,Nothing)}

areaTriangle :: Base -> Height -> Float 
areaTriangle (Base b) (Height h) = (1 / 2) * b * h 

getArea :: Triangle -> Maybe Float 
getArea t = 
    case getSides t of 
        (base, Just h, (_, _))  -> Just (areaTriangle base h)
        (_, _ , (Just x, Just y)) ->             
            if isEqualatiral t 
            then areaOfEqualatiral t 
            else 
                if isIsosceles t 
                then areaOfIsoceles t 
                else 
                    areaOfScalene t 
        ((Base b), _, (Just x, Nothing))  -> 
            case angles t of 
                (Nothing,Nothing,Nothing) -> Nothing
                (Just c, _, _) -> 
                    case c of 
                        (TD d) -> Just $ (1/2) * b * x * (sine d)
                        (TR r) -> Just $ (1/2) * b * x * (sine r)
                _              -> Nothing 
        ((Base b), _ , (_, Just y))  -> 
            case angles t of 
                (Nothing, Nothing, Nothing) -> Nothing 
                (_, _, Just a) -> 
                    case a of 
                        (TD d) -> Just $ (1/2) * b * y * (sine d)
                        (TR r) -> Just $ (1/2) * b * y * (sine r)
        _    -> Nothing 
              

             
areaOfIsoceles :: Triangle -> Maybe Float 
areaOfIsoceles t = 
    case (getSides t) of 
        (_, _, (Just x, Just y)) -> 
           Just $ (1/4) * x * (sqrt ((4 * (x ^ 2)) - (y ^ 2)) )
        _ -> Nothing 
areaOfEqualatiral :: Triangle -> Maybe Float 
areaOfEqualatiral triangle = 
    case (getSides triangle) of 
        (_, _, (Just x, _)) -> 
           Just (((sqrt 3) / 4) * x ^ 2)
        _ -> Nothing 
areaOfScalene :: Triangle -> Maybe Float
areaOfScalene triangle = 
    case (getSides triangle) of 
        ((Base b), _, (Just x, Just y)) -> 
            let s = (b + x + y) / 2 
            in Just $ sqrt (s * (s - b) * (s - x) * (s - y))
----------------------------------------------
getSides :: Triangle -> (Base,Maybe Height,(Maybe Float, Maybe Float))
getSides t = 
    (base t, height t, (fst $ sides t, snd $ sides t))
--------------------------------------------
isEqualatiral :: Triangle -> Bool 
isEqualatiral triangle = 
    case getSides triangle of 
        ((Base b), _ , (Just x, Just y)) -> 
            case angles triangle of 
                (Nothing, Nothing, Nothing) -> 
                   (b == x) && (b == y) 
isIsosceles  :: Triangle -> Bool 
isIsosceles triangle = 
    case (not $ isEqualatiral triangle) of 
        True -> case getSides triangle of 
                 ((Base b), _ , (Just x, Just y)) -> 
                    ((b == x) || (x == y) || (b == y))
        _    -> False 

isScalene :: Triangle -> Bool 
isScalene triangle = 
    (not $ isEqualatiral triangle) && (not $ isIsosceles triangle)
--------------------------------------------






