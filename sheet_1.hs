{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Data.Char (toLower)
--Exercise_2

fl :: [Double] -> [Int]
fl xs = map floor xs
--
f1_2 :: [Double] -> [Int]
f1_2 xs = [floor x | x <- xs]

d11 :: Int -> Bool
d11 n = n `mod` 11 == 0

toLowerCase :: String -> String
toLowerCase = map toLower 

-- Exercise 3 
map_1 :: (a->b) -> [a] -> [b]
map_1 _ [] = []
map_1 f(x:xs) = f x : map_1 f xs 

--Exercise 4 

f :: Eq a => (a-> Bool) -> [a] ->[Int]
f p liste = map (\x -> if p x then 1 else 0 ) liste


f_2 :: Eq a => (a -> Bool) -> [a] -> [a]
f_2 p liste = filter p liste


filter' :: (Eq a) => (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]


-- Exercise 5
swap :: (a, b) -> (b, a)
swap p = (snd p, fst p) 

swap_1 :: (a,b) ->(b,a)
swap_1  (x,y) = (y,x)

apply :: a -> (a -> b) -> b
apply a f = f a

compose :: (a -> b) -> (b -> c) -> a -> c
compose f g x = g (f x) --one also can write dollar signs 
--Beispiel compose (+2) (*5) 3


-- Exercise 6
-- f: elem, notElem, allEqual
-- g: map, map then reverse
-- h: sort, sort then reverse


