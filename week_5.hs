import Data.List
data Set a = Set [a] deriving Show


--Conversion
unSet :: Set a->[a]
unSet (Set xs) = xs

toSet :: Eq a => [a] -> Set a 
toSet xs = Set (nub xs)


--Check wether is an element of a Set 
inSet :: a -> Set a -> Bool 
inSet x s = x 