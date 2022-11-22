data Punct = Pt [Int]

instance Show Punct where
  show(Pt []) = "()"
  show (Pt l) = "(" ++ (tail (foldr (\x y -> "," ++ (show x) ++ y) ")" l))

p1 = Pt [1,2,3]

data Arb = Vid | F Int | N Arb Arb
    deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Punct where
    toArb (Pt []) = Vid
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))
    fromArb (Vid) = (Pt []) 
    fromArb (N (F x) a) = (Pt ([x] ++ ( (\(Pt l) -> l) (fromArb a) )))

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
    perimeter :: (Floating a) => g a -> a
    area :: (Floating a) => g a -> a

instance GeoOps Geo where
  perimeter (Square a) = 4*a
  perimeter (Rectangle a b) = 2*(a + b)
  perimeter (Circle a) = 2 * (pi) * a
  area (Square a) = a*a
  area (Rectangle a b) = a * b
  area (Circle a) = (pi) * a * a

patrat = Square (3::Double)
dreptunghi = Rectangle 3.0 3.0
cerc = Circle 2.0

instance (Floating a, Eq a) => Eq (Geo a) where
    a == b = (perimeter a) == (perimeter b) 
