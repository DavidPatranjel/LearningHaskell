import Data.Char

isVowel :: Char -> Bool
isVowel x = case x of
    'a' -> True
    'e' -> True
    'i' -> True
    'o' -> True
    'u' -> True
    'A' -> True
    'E' -> True
    'I' -> True
    'O' -> True
    'U' -> True
    otherwise -> False

pasareasca :: String -> String
pasareasca "" = []
pasareasca (h:t)
    | isVowel h =  [h] ++ t'
    | isAlpha h = [h] ++ "P" ++ [h] ++ t'
    | otherwise = [h] ++ t'
    where t' = pasareasca t

pasareascaM :: String -> String
pasareascaM xs = do
    x <- xs
    if isVowel x then
        return x
    else if isAlpha x then
            [x] ++ "P" ++ [x]
        else return x 