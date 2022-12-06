newtype Identity a = Identity a
    deriving Show
instance Functor Identity where
    fmap f (Identity x) = (Identity (f x))

data Pair a = Pair a a
    deriving Show
instance Functor Pair where
    fmap f (Pair x y) = (Pair (f x) (f y))

data Constant a b = Constant b
    deriving Show
instance Functor (Constant a) where
    fmap f (Constant x) = (Constant (f x))

data Two a b = Two a b
    deriving Show
instance Functor (Two a) where
    fmap f (Two x y) = (Two x (f y))

data Three a b c = Three a b c
    deriving Show
instance Functor (Three a b) where
    fmap f (Three x y z) = (Three x y (f z))

data Three' a b = Three' a b b
    deriving Show
instance Functor (Three' a) where
    fmap f (Three' x y z) = (Three' x (f y) (f z))

data Four a b c d = Four a b c d
    deriving Show
instance Functor (Four a b c) where
    fmap f (Four x y z t) = (Four x y z (f t))

data Four'' a b = Four'' a a a b
    deriving Show
instance Functor (Four'' a) where
    fmap f (Four'' x y z t) = (Four'' x y z (f t))

data Quant a b = Finance | Desk a | Bloor b
    deriving Show
instance Functor (Quant a) where
    fmap f Finance = Finance
    fmap f (Desk x) = (Desk x)
    fmap f (Bloor y) = (Bloor (f y))

data LiftItOut f a = LiftItOut (f a)
    deriving Show
instance (Functor f) => Functor (LiftItOut f) where
    fmap func (LiftItOut fx) = (LiftItOut (fmap func fx))

data Parappa f g a = DaWrappa (f a) (g a)
    deriving Show
instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap func (DaWrappa fx gx) = (DaWrappa (fmap func fx) (fmap func gx))

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    deriving Show
instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap func (IgnoringSomething fx gx) = (IgnoringSomething fx (fmap func gx))


data Notorious g o a t = Notorious (g o) (g a) (g t)
    deriving Show
instance (Functor g) => Functor (Notorious g o a) where
    fmap func (Notorious gc ga gt) = (Notorious gc ga (fmap func gt))

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving Show
instance Functor (GoatLord) where
    fmap func NoGoat = NoGoat
    fmap func (OneGoat a) = (OneGoat (func a))
    fmap func (MoreGoats x y z) = (MoreGoats (fmap func x) (fmap func y) (fmap func z))

data TalkToMe a = Halt | Print String a | Read (String -> a) 

    
instance Functor (TalkToMe) where
    fmap func Halt = Halt
    fmap func (Print str a) = (Print str (func a))
    fmap func1 (Read  func2) = let func3 = func1.func2 
                                in (Read func3)

--TESTARE
double :: Int -> Int
double a = 2 * a

fct :: String -> Int 
fct str = length str 

talkToMe = Read fct 
getVal (Read f) x = f x 