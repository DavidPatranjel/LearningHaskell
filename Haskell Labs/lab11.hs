{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"

Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}
data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

instance Functor List where
    fmap _ (Nil) = Nil
    fmap f (Cons a l) = (Cons (f a) (fmap f l)) 
instance Applicative List where
    pure x = Cons x (Nil)
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f l) <*> x = (concatlist (fmap f x) (l <*> x))

concatlist :: (List a) -> (List a) -> (List a)
concatlist Nil Nil = Nil
concatlist Nil l = l
concatlist (Cons a l) x = Cons a (concatlist l x)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty s = if length s == 0 then Nothing
            else (Just s)

noNegative :: Int -> Maybe Int
noNegative n = if n < 0 then Nothing
                else (Just n) 

test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5 

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString _name _age _weight = case (noEmpty _name) of
                                        (Nothing) -> Nothing
                                        (Just a) -> case (noNegative _age) of
                                                    (Nothing) -> Nothing
                                                    (Just b) -> case (noNegative _weight) of
                                                                (Nothing) -> Nothing
                                                                (Just c) -> Just (Cow {name = a, age = b, weight = c})

cowFromString':: String -> Int -> Int -> Maybe Cow
cowFromString' _name _age _weight = Cow <$> (noEmpty _name) <*> (noNegative _age) <*> (noNegative _weight)

test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})
test25 = cowFromString' "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength x s = if (length s) > x then Nothing
                        else (Just s) 

test31 = validateLength 5 "abc" == Just "abc"
mkName :: String -> Maybe Name
mkName s = case (validateLength 30 s) of
                Nothing -> Nothing
                (Just a) -> Just (Name a)

mkAddress :: String -> Maybe Address
mkAddress s = case (validateLength 100 s) of
                Nothing -> Nothing
                (Just a) -> Just (Address a)

test32 = mkName "Gigel" ==  Just (Name "Gigel")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

mkPerson :: String -> String -> Maybe Person
mkPerson  _name _address = case (mkName _name) of
                                (Nothing) -> Nothing
                                (Just a) -> case (mkAddress _address) of
                                                (Nothing) -> Nothing
                                                (Just b) -> Just (Person a b)

mkPerson' :: String -> String -> Maybe Person
mkPerson' _name _address = Person <$> (mkName _name) <*> (mkAddress _address)

test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))
test35 = mkPerson' "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))

double :: Int -> Int
double x = 2 * x