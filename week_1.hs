import Data.ByteString.Builder (FloatFormat)
hello = putStrLn "Hello"

doubleMe :: Int -> Int
doubleMe x = x + 2

doubleMe_1 :: Float->Float
doubleMe_1 x = x * 2

tripleMe :: Int -> Int
tripleMe y = y * 3

doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = 
    if x>100
        then x 
        else 2*x

addUs :: Int -> Int -> Int
addUs x y = x + y 

doubleMe_2 :: Num a => a -> a
doubleMe_2 x = x + x 
