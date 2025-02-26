hello = putStrLn "Hello"

doubleMe :: Int -> Int
doubleMe x = x * 2

tripleMe :: Int -> Int
tripleMe y = y * 3

doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = 
    if x>100
        then x 
        else 2*x

-- test

