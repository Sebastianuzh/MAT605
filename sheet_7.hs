import Data.List (nub)

-- Datatype for Sets
data Set a = Set [a] deriving (Show)

instance (Eq a) => Eq (Set a) where
  (==) :: (Eq a) => Set a -> Set a -> Bool
  s == t = s `subSet` t && t `subSet` s

unSet :: Set a -> [a]
unSet (Set xs) = xs

toSet :: (Eq a) => [a] -> Set a
toSet xs = Set (nub xs)

inSet :: (Eq a) => a -> Set a -> Bool
inSet x s = x `elem` unSet s

subSet :: (Eq a) => Set a -> Set a -> Bool
subSet (Set []) _ = True
subSet (Set (x : xs)) t = x `inSet` t && Set xs `subSet` t

unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet s t = toSet (unSet s ++ unSet t)

intersectSet :: (Eq a) => Set a -> Set a -> Set a
intersectSet s t = toSet [x | x <- unSet s, x `inSet` t]


-- Ex 1

data Fun' a b = Fun' (Set a) (Set b) (Set (a, b)) deriving (Show)

-- Record syntax
data Fun a b = Fun
  { dom :: Set a,
    cod :: Set b,
    pairs :: Set (a, b)
  }
  deriving (Show)

g = Fun (toSet [1, 2]) (toSet ['a', 'b', 'c']) (toSet [(1, 'b'), (2, 'c')])

-- Ex 2
getPairs :: Fun a b -> [(a, b)]
getPairs (Fun s t p) = unSet p

numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub

numYs :: (Eq a, Eq b) => Fun a b -> a -> Int
numYs f x = numUnique [y' | (x', y') <- getPairs f, x' == x]

isFun :: (Eq a, Eq b) => Fun a b -> Bool
isFun f =
  imFun f `subSet` cod f
    && toSet [x | (x, y) <- getPairs f] `subSet` dom f
    && and
      [numYs f x == 1 | x <- unSet (dom f)]

-- We haven't checked if the pairs actually lie in domain x codomain

-- Ex 3
composeFun :: (Eq a, Eq b, Eq c) => Fun b c -> Fun a b -> Fun a c
composeFun g f = Fun (dom f) (cod g) p
  where
    p = toSet [(x, z') | (x, y) <- getPairs f, (y', z') <- getPairs g, y == y']

-- Ex 4
imFun :: (Eq b) => Fun a b -> Set b
imFun f = toSet [y | (x, y) <- getPairs f]

-- Ex 5
isSurj :: (Eq b) => Fun a b -> Bool
isSurj f = imFun f == cod f

numXs :: (Eq a, Eq b) => Fun a b -> b -> Int
numXs f y = numUnique [x | (x, y') <- getPairs f, y == y']

isInj :: (Eq a, Eq b) => Fun a b -> Bool
isInj f = and [numXs f y <= 1 | y <- unSet $ cod f]

-- Ex 6
toFun :: (Eq a, Eq b) => (a -> b) -> [a] -> [b] -> Fun a b
toFun f s t = Fun (toSet s) (toSet t) p
  where
    p = toSet [(x, f x) | x <- s]

fromFun :: (Eq a) => Fun a b -> (a -> b)
fromFun f x = head [y' | (x', y') <- getPairs f, x' == x]