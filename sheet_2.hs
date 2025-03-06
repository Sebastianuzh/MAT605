--Exercise 1 


funktion_1 :: (a -> b) -> [a] -> [b]
funktion_1 f [] = []            -- Base case: empty list
funktion_1 f (x:xs) = f x : funktion_1 f xs  -- Recursive case: apply f to x and recurse on xs

--Exercise 2

filter_1 :: (a->Bool) -> [a]->[a]
filter_1 f [] = []
filter_1 f (x:xs)
    |f x = x : filter_1 f xs
    |otherwise = filter_1 f xs

--Exercise 3 

zip_1 ::[a]->[b] ->[(a,b)]
zip_1 _[]=[]
zip_1 []_ = []
zip_1 (x:xs) (y:ys) = (x, y) : zip_1 xs ys

