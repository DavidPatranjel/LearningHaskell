import Data.Char
--ex1
isVowel :: Char -> Bool
isVowel x = case x of
    'a' -> True
    'e' -> True
    'i' -> True
    'o' -> True
    'u' -> True
    otherwise -> False

vowels :: String -> Int
vowels "" = 0
vowels (h:t)
    | isVowel h =  1 + t'
    | otherwise = t'
    where t' = vowels t

nrVocale :: [String] -> Int
nrVocale [] = 0 
nrVocale (h:t)
 | (reverse h == h) = (vowels h) + t'
 | otherwise = t'
 where t' = nrVocale t

--ex2
f :: Int -> [Int] -> [Int]
f x [] = []
f x (h:t)
 | (h `mod` 2 == 0) = h : x : t'
 | otherwise = h : t'
 where t' = f x t

 --model
semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]

--ex3
divizori :: Int -> [Int]
divizori n = [x | x <- [1..n], let r = n `mod` x, r == 0]

--ex4
listadiv :: [Int] -> [[Int]]
listadiv n = [divizori x | x <- n]

--ex5
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b n = [x | x <- n, a <= x && x <= b]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec a b [] = []
inIntervalRec a b (h:t)
 | a <= h && h <= b = h : t'
 | otherwise = t'
 where t' = inIntervalRec a b t

--ex6
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
 | h > 0 = 1 + t'
 | otherwise = t'
 where t' = pozitiveRec t

pozitiveComp :: [Int] -> Int
pozitiveComp n = (length [x | x <- n, x > 0])

--ex7
pseudoPozitiiImp :: Int -> [Int] -> [Int]
pseudoPozitiiImp x [] = []
pseudoPozitiiImp x (h:t)
 | (h `mod` 2 == 1) = x : t'
 | otherwise = t'
 where t' = pseudoPozitiiImp (x + 1) t

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec n = (pseudoPozitiiImp 0 n)

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp n = [x-1 | (x,y) <- (zip [1..(length n)] n), (y `mod` 2 == 1)]

--ex8
multDigitsComp :: String -> Int
multDigitsComp n = (product [digitToInt x | x <- n, (isDigit x)] )

multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (h:t)
 | (isDigit h) = (digitToInt h) * t'
 | otherwise = t'
 where t' = multDigitsRec t