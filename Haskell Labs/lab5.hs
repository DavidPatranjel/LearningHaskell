ex1 :: [Int] -> Int
ex1 l = (foldr (+) 0 (map (^2) (filter odd l)))

ex2 :: [Bool] -> Bool
ex2 l = (foldr (&&) True l)

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies prop l = (foldr (\ x y -> y && (prop x)) True l)


anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies prop l = (foldr (\ x y -> y || (prop x)) False l)

mapFoldr:: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr (\ x y -> [f x] ++ y) [] xs

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p xs = foldr (\ x y -> if p x then [x] ++ y else y ) [] xs

listToInt :: [Integer] -> Integer
listToInt l = foldl (\ a b -> a * 10 + b) 0 l 

rmChar :: Char -> String -> String
rmChar c s = filter (/= c) s

rmCharsRec :: String -> String -> String
rmCharsRec [] b = b
rmCharsRec (h:t) b = rmCharsRec t (rmChar h b)
    
rmCharsFold :: String -> String -> String
rmCharsFold a b = foldr (\ x y -> if x `elem` a then y else [x] ++ y ) [] b
