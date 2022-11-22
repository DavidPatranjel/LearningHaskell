factori :: Int -> [Int]
factori n = [x | x <- [1..n], n `mod` x == 0]

prim :: Int ->Bool
prim n = (length (factori n) == 2)

numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 a b c = [(x,y,z) | (x, (y, z)) <- (zip a (zip b c))]

firstE1 :: [(a, b)] -> [a]
firstE1 n = (map (\ (a,b) -> a) n )

sumList :: [[Int]] -> [Int]
sumList a = map (sum) a

modifElem :: Int -> Int
modifElem x = if x `mod` 2 == 0 then x `div` 2
                else x*2

prel2 :: [Int] -> [Int]
prel2 l =  map (modifElem) l

charInList :: Char -> [String] -> [String]
charInList c l = filter (c `elem`) l

sqrOdd :: [Int] -> [Int]
sqrOdd l = map (^ 2) (filter odd l)

sqrOdd2 :: [Int] -> [Int]
sqrOdd2 l = map (\ (a,b) -> a^2)  (filter (\ (a,b) -> b `mod` 2 == 1)  (zip l [0..]))

trueElimCons :: String -> String
trueElimCons ""=[]
trueElimCons (h:t)
    | h `elem` "aeiouAEIOU" = h:t'
    | otherwise = t'
    where t' = trueElimCons t

trueElimCons2 :: String -> String
trueElimCons2 l = filter (`elem` "aeiouAEIOU") l

elimCons :: [String] -> [String]
elimCons l = map (trueElimCons) l

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (h:t)
    | f h = h:t'
    |otherwise = t'
    where t' = myfilter f t

mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (h:t) = (f h):t'
    where t' = mymap f t

