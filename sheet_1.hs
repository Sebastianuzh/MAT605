{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
import Data.Char (toLower)
--Exercise_2

fl :: [Double] -> [Int]
fl = map floor

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