data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

instance Show Expr where
  show (Const a) = show a
  show (a :+: b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (c :*: d) = "(" ++ show c ++ "*" ++ show d ++ ")"

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)

evalExp :: Expr -> Int
evalExp (Const a) = a 
evalExp (a :+: b) = (evalExp a) + (evalExp b)
evalExp (a :*: b) = (evalExp a) * (evalExp b)
  
exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16

evalArb :: Tree -> Int
evalArb (Lf a) = a
evalArb (Node Add a b) = (evalArb a) + (evalArb b)
evalArb (Node Mult a b) = (evalArb a) * (evalArb b) 




arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16


expToArb :: Expr -> Tree
expToArb (Const a) = (Lf a)
expToArb (a :+: b) = (Node Add (expToArb a) (expToArb b)) 
expToArb (a :*: b) = (Node Mult (expToArb a) (expToArb b)) 



class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  keys x = [a | (a,b) <- (toList x)] 
  values :: c key value -> [value]
  values x = [b | (a,b) <- (toList x)] 
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value
  fromList [] =empty
  fromList ((x,y):xs)= insert x y t'
    where t' = fromList xs

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }
lista = PairList [(1,"abc"),(2,"dfd")]

instance Collection PairList where
  empty = PairList []
  singleton k v = PairList [(k,v)]
  insert k v (PairList l) = (PairList (l ++ [(k,v)]) )
  clookup k (PairList l) = let aux = (filter (\(a,b) -> a == k) l)
                            in if length aux == 0 then Nothing
                              else Just (snd (head aux))
  delete x (PairList []) = (PairList [])
  delete x (PairList (y:ys)) = if x == (fst y) then (delete x (PairList ys))
                                else (PairList ([y] ++ (getPairList (delete x (PairList ys) ) )) )
  toList (PairList l) = l 
  


data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare
      deriving Show

instance Collection SearchTree where
  empty = Empty
  singleton k v = BNode Empty k (Just v) Empty
  insert k v Empty = singleton k v
  insert k v (BNode a cheie valoare b) = if k <= cheie then (BNode (insert k v a) cheie valoare b)
                                          else  (BNode a cheie valoare (insert k v b))
  clookup k Empty = Nothing
  clookup k (BNode a cheie valoare b) = if k == cheie then valoare
                                        else if k < cheie then (clookup k a)
                                            else (clookup k b)

  
  toList Empty = []
  toList (BNode a cheie valoare b) = case valoare of 
                                        Nothing -> (toList a) ++ (toList b) 
                                        Just x -> (toList a) ++ [(cheie, x)] ++ (toList b) 
tree = BNode (BNode Empty 1 (Just 3) Empty) 3 (Just 10) (BNode Empty 5 (Just 2) Empty)