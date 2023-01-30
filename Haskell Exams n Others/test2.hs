--Examen 2 
--Link https://drive.google.com/drive/folders/16kzWRTRUa0F9CfarjUIKMLWwYle73LY3?usp=share_link
f :: String -> String -> String
f [] _ = []
f _ [] = []
f (a:ax) (b:bx) = if a == b then [a] ++ (f ax bx)
                    else []

g :: [Int] -> [Int] -> (Either String Int)
g l r = if (length l) /= (length r) then (Left "lungimi diferite!")
        else let lista = (zip l r)
            in foldr (\(a, b) (Right y) -> (Right (a * a * b * b + y))) (Right 0) lista

data PairInt = P Int Int deriving Show
data MyList = L [PairInt] deriving Show
data Exp = I Int | Add Exp Exp | Mul Exp Exp deriving Show

class MyClass m where
    toExp :: m -> Exp

instance MyClass MyList where
    toExp (L []) = (I 1)
    toExp (L ((P a b):t)) = (Mul (Add (I a) (I b)) (toExp (L t)))

ex2 = (L [P 1 2 , P 2 3, P 5 3])

eval :: MyList -> Int
eval lista = (calcul (toExp lista))

calcul :: Exp -> Int
calcul (I n) = n
calcul (Add a b) = (calcul a) + (calcul b)
calcul (Mul a b) = (calcul a) * (calcul b)