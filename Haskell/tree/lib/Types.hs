module Types where 
import Data.List (partition)

data Tree a = Node (Tree a) a (Tree a)
            | Leaf 
            deriving (Show, Eq)

fromList :: Ord a => [a] -> Tree a  
fromList [] = Leaf 
fromList [x] = Node Leaf x Leaf
fromList (x : xs) = 
    let doesDoesNot = (partition (\y -> (compare x y)== GT) xs)
    in  Node (fromList $ fst doesDoesNot) x (fromList $ snd doesDoesNot)

toList :: Tree a -> [a]
toList Leaf = [] 
toList (Node (tree1) x (tree2)) = x : ((toList tree1) ++ (toList tree2))

headTree :: Tree a -> a 
headTree Leaf = error "no head"
headTree (Node _ x _ ) = x 

tailTree :: Ord a => Tree a -> Tree a 
tailTree Leaf = Leaf 
tailTree  tree = fromList (tail (toList tree))





    