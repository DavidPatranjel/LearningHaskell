pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
--fct  mx =  mx  >>= (\x -> Just (pos x))
fct mx = do
  x <- mx
  return (pos x)

addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = do 
  a <- mx
  b <- my
  return (a + b)

addMm :: Maybe Int -> Maybe Int -> Maybe Int
addMm Nothing a = a
addMm b Nothing = b
addMm (Just a) (Just b) = Just (a + b)

--cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))
cartesian_product xs ys = do
  x <- xs
  y <- ys
  return (x, y)

--prod f xs ys = [f x y | x <- xs, y<-ys]
prod f xs ys = do
  x <- xs
  y <- ys
  return (f x y)

{-
myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)
-}
myGetLine :: IO String
myGetLine = do
    x <- getChar
    if x == '\n' then
      return []
    else do
      b <- myGetLine
      return (x:b)
      
prelNo noin =  sqrt noin

{-
ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout
-}

ioNumber = (readLn :: (IO Float)) >>= \noin -> putStrLn("Intrare\n" ++ (show noin)) >> return (prelNo noin) >>= \noout -> putStrLn "Iesire" >> print noout


newtype WriterS a = Writer { runWriter :: (a, [String]) } 


instance  Monad WriterS where
  return va = Writer (va, [])
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: [String] -> WriterS () 
tell log = Writer ((), log)
  
logIncrement :: Int  -> WriterS Int
logIncrement x = do
        tell ( ["increment:" ++ show x ++ "\n"] )
        return (x + 1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
          y <- logIncrement x
          logIncrement y

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n =  if n == 1 
                        then logIncrement x
                        else do
                          y <- logIncrement x
                          logIncrementN y (n - 1)
                      

data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN (Person name _) = "NAME: " ++ name
showPersonA :: Person -> String
showPersonA (Person _ age) = "AGE: " ++ show age

showPerson :: Person -> String
showPerson person = "(" ++ (showPersonN person) ++ ", " ++ (showPersonA person) ++ ")"

newtype Reader env a = Reader { runReader :: env -> a }

ask :: Reader env env
ask = Reader id

instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env

instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma    

mshowPersonN ::  Reader Person String
mshowPersonN = do
          (Person name _) <- ask
          return ("NAME: " ++ name)
mshowPersonA ::  Reader Person String
mshowPersonA = do
          (Person _ age) <- ask
          return ("AGE: " ++ show age) 
mshowPerson ::  Reader Person String
mshowPerson = do
          name <- mshowPersonN
          age <- mshowPersonA
          return ("(" ++ name ++ ", " ++ age ++ ")") 
