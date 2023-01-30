--Examen Model
---Link: https://drive.google.com/drive/folders/16kzWRTRUa0F9CfarjUIKMLWwYle73LY3?usp=share_link
import Data.Functor

data Point = Pt [Int]
    deriving Show

data Arb = Empty | Node Int Arb Arb
    deriving Show

insert :: Int -> Arb -> Arb
insert a Empty = Node a Empty Empty
insert a (Node n l r) = if a < n then (Node n (insert a l) r)
                        else (Node n l (insert a r))
getPoint :: Point -> [Int]
getPoint (Pt lista) = lista

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Point where
    toArb (Pt []) = Empty
    toArb (Pt (x:xs)) = insert x (toArb (Pt xs))
    fromArb Empty = (Pt [])
    fromArb (Node elem l r) = Pt((getPoint(fromArb l)) 
                            ++ [elem] 
                            ++ (getPoint(fromArb r)))

exemplu = Pt [3, 9, 5, 6, 1, 2]
ex2 = (Node 2 (Node 1 Empty Empty) (Node 6 (Node 5 (Node 3 Empty Empty) Empty) (Node 9 Empty Empty)))


getFromInterval :: Int -> Int ->[Int] -> [Int]
getFromInterval a b l = [x | x <- l, x >= a && x <= b]


getFromInterval2 :: Int -> Int ->[Int] -> [Int]
getFromInterval2 a b l = do
                         x <- l 
                         if x >= a && x <= b
                            then [x] --return x
                         else []


newtype ReaderWriter env a = RW {getRW :: env -> (a, String)}

instance Functor (ReaderWriter env) where
    fmap f ma = pure f <*> ma

instance Applicative (ReaderWriter env) where
    pure = return
    mf <*> ma = do
            f <- mf
            a <- ma
            return (f a)

instance Monad (ReaderWriter env) where
    return x = RW(const (x, ""))
    ma >>= k = RW f
                where f env =   let (va, str1) = getRW ma env
                                    (vb, str2) = getRW (k va) env
                                in (vb, str1 ++ str2)