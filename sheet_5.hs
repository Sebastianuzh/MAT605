import Data.List (nub)

data Set a = Set [a] deriving(Show)

instance (Eq a) => Eq (Set a) where
  (==) :: Set a -> Set a -> Bool
  s == t = (s `subSet` t) && (t `subSet` s)

-- Exercise 1
-- (i) Set ['a','b','c'] :: Set Char
-- (ii) Duplicates are retained
-- (iii) Error: all elements need be of the same type

card :: Set a -> Int
card (Set xs) = length xs

-- Exercise 2j
unSet :: Set a -> [a]
unSet (Set xs) = xs

toSet :: (Eq a) => [a] -> Set a
toSet xs = Set (nub xs)

-- Exercise 3
inSet :: (Eq a) => a -> Set a -> Bool
inSet x (Set xs) = x `elem` xs
--Beispiel inSet 5 (Set [1,2,3,5])


subSet :: (Eq a) => Set a -> Set a -> Bool
subSet (Set []) _ = True
subSet (Set (x : xs)) t = x `inSet` t && Set xs `subSet` t

subSet' :: (Eq a) => Set a -> Set a -> Bool
subSet' (Set xs) (Set ys) = and [x `elem` ys | x <- xs]

--Erklärung: For each x in xs, checks if x is in ys Then applies and to that list: If all elements are True, it returns True

--Beispiel subSet'( Set[1])( Set[1,2])

subSet'' :: (Eq a) => Set a -> Set a -> Bool
subSet'' s t = all (`inSet` t) (unSet s)

-- Aside: Anonymous functions
add1 :: Integer -> Integer
add1 = \x -> x + 1

-- (`inset` t ) is equivalent to \x -> x `inSet` t


-- Exercise 4
unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet (Set xs) (Set ys) = toSet (xs ++ ys)
--Beispiel toSet([1,1,1,1,1]++[2,3,1])

intersectSet :: (Eq a) => Set a -> Set a -> Set a
intersectSet (Set xs) (Set ys) = toSet [x | x <- xs, x `elem` ys]

intersectSet' :: (Eq a) => Set a -> Set a -> Set a
intersectSet' (Set xs) (Set ys) = toSet $ filter (`elem` ys) xs
--filter (elem ys) xs Filters elements in xs Keeps only those x for which x \elem` ys isTrue` So: elements from xs that are also in ys
