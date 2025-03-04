--Type Variables
fst :: (a,b) -> a 
fst (x,y)=x 

--Type classes 

doubleMe :: Num a => a -> a
doubleMe x = x + x


--Eq (==)
--ord (grösser kleiner gleich )
--Num 
--Show
--Integral Floating Fractional

isEqual :: Eq a => a -> a -> Bool
isEqual x y = x==y 

--fromIntegral
l = [1,2,3]
len = fromIntegral(length l ) / 4.5

-- pattern Matching

sayNumber :: Int -> String
sayNumber x = if x == 1 then  "This is one" else "This is a Number"


sayNumber_1 :: Int -> String
sayNumber_1 1 = "This is a Number"
sayNumber_1 2 = "This is Two"
sayNumber_1 _ = "This is a Number" --blank for a random variable if not specific


factorial :: (Integral a)  => a ->a
factorial 0 = 1 
factorial n = n * factorial (n-1)

addVectors :: (Double,Double) -> (Double, Double) -> (Double,Double)
addVectors (x_1, x_2) (y_1, y_2) = (x_1 + y_1, x_2 + y_2)

first :: (a,b,c) -> a 
first (x,y,z) = x 

head_1 :: [a] -> a 
head_1 [] = error "empty list"
head_1 (x:xs) = x


-- Exercise: Write function that calculate scalar product

sayLength :: [a] -> String
sayLength [] = "Zero"
sayLength [x] = "one"
sayLength [x,y] = "two"
sayLength _ = " Longer than two"

sayLength_1 :: [a] -> String
sayLength_1 [] = "zero"
sayLength_1 (x:[]) = "one"
sayLength_1 (x:y:[]) = "two"
sayLength_1 _ = " Longer than two"


length_1 :: [a] -> Int 
length_1 [] = 0 
length_1 (x:xs) = length_1 xs +1 


length_2 :: [a] -> Int
length_2 []= 0 
length_2 xs = length_2 (tail xs) + 1 

sum_1 :: (Num a) => [a] ->a 
sum_1 []= 0 
sum_1 (x:xs) = x + sum_1 xs 


--Gards

grade :: Double -> String
grade p  
    |p <= 5 = "Fail"
    |p<= 10 ="C"
    |p<= 15 ="B"
    |otherwise = "A"


cylinder :: Double -> Double -> Double 
cylinder r h = 2 * topArea + sideArea
    where 
        topArea = pi * r * r
        sideArea = h * 2 * pi * r

