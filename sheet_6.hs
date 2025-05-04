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





-- Relations R: A -> A
type Rel a = Set (a, a)

-- Relations R: A -> B
type Rel' a b = Set (a, b)

-- Relation R: A -> A as function
type FunRel a = a -> a -> Bool

-- Relations R: A -> B as functions
type FunRel' a b = a -> b -> Bool

-- Domain / range of a relation
domRel :: (Eq a) => Rel a -> Set a
domRel r = toSet [fst xs | xs <- unSet r]
--Beispiel domRel (Set [(1,2), (3,4), (1,3)])

domRel' :: (Eq a) => Rel a -> Set a
domRel' r = toSet [x | (x, _) <- unSet r]

ranRel :: (Eq a) => Rel a -> Set a
ranRel r = toSet [snd xs | xs <- unSet r]

ranRel' :: (Eq a) => Rel a -> Set a
ranRel' r = toSet [y | (_, y) <- unSet r]

-- Check if (x,y) is in relation
inRel :: (Eq a) => (a, a) -> Rel a -> Bool
inRel xs r = xs `inSet` r
--Beispiel inRel(1,2)(Set [(1,2), (3,4), (1,3)])

inRel' :: (Eq a) => (a, a) -> Rel a -> Bool
inRel' = inSet

-- Properties:
-- Reflexivity
reflRel :: (Eq a) => Set a -> Rel a -> Bool
reflRel a r = and [(x, x) `inRel` r | x <- unSet a]
--Beispiel reflRel (Set [1,3,2]) (Set [(1,1), (2,2)])

-- Symmetry
symRel :: (Eq a) => Rel a -> Bool
symRel r = and [(y, x) `inRel` r | (x, y) <- unSet r]

-- Transitivity
transRel :: (Eq a) => Rel a -> Bool
transRel r =
  and
    [ (x, z') `inRel` r
      | (x, y) <- unSet r,
        (y', z') <- unSet r,
        y == y'
    ]

-- Relation composition
infix 5 @@

(@@) :: (Eq a) => Rel a -> Rel a -> Rel a
r @@ s = toSet [(x, z') | (x, y) <- unSet r, (y', z') <- unSet s, y == y']