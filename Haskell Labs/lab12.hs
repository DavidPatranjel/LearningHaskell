import Data.Monoid

elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 elem obj = foldr (\x y -> y || (elem == x)) False obj

null1 :: (Foldable t) => t a -> Bool
null1 obj = foldr (\x y -> False) True obj

length1 :: (Foldable t) => t a -> Int
length1 obj = foldr (\x y -> y + 1) 0 obj

toList1 :: (Foldable t) => t a -> [a]
toList1 obj = foldl (\x y -> x ++ [y]) [] obj

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 obj = foldMap id obj

data Constant a b = Constant b
instance Foldable (Constant a) where
    foldMap mon (Constant x) = mon x

data Two a b = Two a b
instance Foldable (Two a) where
    foldMap mon (Two x y) = mon y

data Three a b c = Three a b c
instance Foldable (Three a b) where
    foldMap mon (Three x y z) = mon z

data Three' a b = Three' a b b
instance Foldable (Three' a) where
    foldMap mon (Three' x y z) = (mon y) <> (mon z)

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
    foldMap mon (Four' x y z t) = (mon y) <> (mon z) <> (mon t)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Foldable (GoatLord) where
    foldMap mon NoGoat = mempty
    foldMap mon (OneGoat x) = mon x
    foldMap mon (MoreGoats a b c) = (foldMap mon a) <> (foldMap mon b) <> (foldMap mon c) 