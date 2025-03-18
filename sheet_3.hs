--Exercise_1
{-# LANGUAGE BlockArguments #-}

data CoolBool = Nope | Yup deriving(Show,Eq,Ord) --Eq kann vergleichen, ord gibt eine ordung


--Exercise_2

boolToCool:: Bool -> CoolBool
boolToCool True = Yup
boolToCool False = Nope


coolToBool :: CoolBool -> Bool
coolToBool Yup = True
coolToBool Nope = False



--Exercise_3

coolNot:: CoolBool->CoolBool
coolNot Yup=Nope
coolNot Nope=Yup


coolBoth:: CoolBool->CoolBool->CoolBool
coolBoth Yup Yup = Yup
coolBoth _ _ = Nope


coolEither:: CoolBool->CoolBool->CoolBool
coolEither Nope Nope = Nope
coolEither _ _ = Yup

--Exercise_4

coolAnd::[CoolBool]->CoolBool
coolAnd [] = Yup
coolAnd (x:xs) = if x == Nope then Nope else coolAnd xs 

coolOr::[CoolBool]->CoolBool
coolOr [] = Nope
coolOr (x:xs) = if x== Yup then Yup else coolOr xs

--Alternative 

coolOr_1::[CoolBool]->CoolBool
coolOr_1 [] = Nope
coolOr_1 (Yup:xs) =Yup
coolOr_1 (Nope :xs) = coolOr_1 xs


--Exercise_5

coolElem:: Eq a=>a->[a]->CoolBool
coolElem _ [] = Nope
coolElem a (x:xs) = if x==a then Yup else coolElem a xs
 

--Exercise_6

-- Predicate function: Is the number even?
isEven :: Int -> CoolBool
isEven x = boolToCool (x `mod` 2 == 0)

coolAll :: (a-> CoolBool)-> [a]-> CoolBool
coolAll _ [] = Yup
coolAll p (x:xs) = if p x == Nope then Nope else coolAll p xs 