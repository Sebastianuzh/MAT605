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
