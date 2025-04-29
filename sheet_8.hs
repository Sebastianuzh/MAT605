import Nats

data SignInt = Plus Nat | Minus Nat deriving (Show)

instance Eq SignInt where 
    (==) (Plus Z) (Minus Z) = True 
