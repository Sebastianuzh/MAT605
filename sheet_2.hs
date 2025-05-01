--Exercise 1 


funktion_1 :: (a -> b) -> [a] -> [b]
funktion_1 f [] = []            -- Base case: empty list
funktion_1 f (x:xs) = f x : funktion_1 f xs  -- Recursive case: apply f to x and recurse on xs
--Beispiel funktion_1 (*3) [1,2,3,4,5,6]
--Exercise 2

filter_1 :: (a->Bool) -> [a]->[a]
filter_1 f [] = []
filter_1 f (x:xs)
    |f x = x : filter_1 f xs
    |otherwise = filter_1 f xs
--Beispiel filter_1 ((== 0) . (`mod` 3)) [1,2,3,4,5,6], oder filter_1 (>3) [1,2,3,4,5,6]


--Exercise 3 

zip_1 ::[a]->[b] ->[(a,b)]
zip_1 _[]=[]
zip_1 []_ = []
zip_1 (x:xs) (y:ys) = (x, y) : zip_1 xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys
--Beispiel zipWith' (+) [1, 2, 3] [4, 5, 6]

scalarProd :: (Num a) => [a] -> [a] -> a
scalarProd xs ys = sum $ zipWith' (*) xs ys


-- Exercise 4
flatten' :: [[a]] -> [a]
flatten' [] = []
flatten' (x : xs) = x ++ flatten' xs
--Beispiel flatten' [[1, 2], [3, 4], [5, 6]]


-- Exercise 5
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [2 * x]
doubleEveryOther (x : y : xs) = 2 * x : y : doubleEveryOther xs
--Beispiel doubleEveryOther [1, 2, 3, 4, 5]

-- Exercise 6
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits firstDigits ++ [lastDigit]
  where
    lastDigit = n `mod` 10
    firstDigits = n `div` 10