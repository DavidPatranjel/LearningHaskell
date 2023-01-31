data Piece = One
            | Two
            | Empty
    deriving(Show, Eq)

data Table = Table [Piece] [Piece] [Piece]
    deriving(Show, Eq)

validTable :: Table -> Bool
validTable (Table a b c) = let (x1, y1) = (countPlayer a)
                               (x2, y2) = (countPlayer b)
                               (x3, y3) = (countPlayer c)
                            in length a == 8 && length b == 8 && length c == 8
                                && (x1 + x2 + x3 <= 9) && (y1 + y2 + y3 <= 9)

countPlayer :: [Piece] -> (Int, Int)
countPlayer lista = (length (filter (== One) lista), length (filter (== Two) lista))

data Position = P (Int, Int)

findPiesa :: (Int, Int) -> Table -> Bool
findPiesa (x, y) (Table a b c) = case x of 1 -> (a !! y) /= Empty 
                                           2 -> (b !! y) /= Empty 
                                           3 -> (c !! y) /= Empty 

connected :: (Int, Int) -> (Int, Int) -> Bool
connected (x1, y1) (x2, y2) =   if (abs (x1 - x2) /= 1) then 
                                    if (x1 == x2) && (abs (y1 - y2) == 1 || abs (y1 - y2) == 7)then True
                                    else  False
                                else if (y1 /= y2) then False
                                    else (mod y1 2 == 1)

replaceElem :: [Piece] -> Int -> Piece -> [Piece]
replaceElem xs i x = take i xs ++ [x] ++ drop (i + 1) xs

replace :: Table -> (Int, Int) -> (Int, Int) -> Table
replace (Table a b c) (x1, y1) (x2, y2) = case (x1, x2) of (1, 2) -> let x = a !! y1
                                                                         y = b !! y2
                                                                      in (Table (replaceElem a y1 y) (replaceElem b y2 x) c)
                                                           (2, 1) -> let x = b !! y1
                                                                         y = a !! y2
                                                                      in (Table (replaceElem a y2 x) (replaceElem b y1 y) c)
                                                           (2, 3) -> let x = b !! y1
                                                                         y = c !! y2
                                                                      in (Table a (replaceElem b y1 y) (replaceElem c y2 x))
                                                           (3, 2) -> let x = c !! y1
                                                                         y = b !! y2
                                                                      in (Table a (replaceElem b y2 x) (replaceElem c y1 y))
                                                           (1, 1) -> let x = a !! y1
                                                                         y = a !! y2
                                                                         aux = (replaceElem a y2 x)
                                                                      in (Table (replaceElem aux y1 y) b c)
                                                           (2, 2) -> let x = b !! y1
                                                                         y = b !! y2
                                                                         aux = (replaceElem b y2 x)
                                                                      in (Table a (replaceElem aux y1 y) c)
                                                           (3, 3) -> let x = c !! y1
                                                                         y = c !! y2
                                                                         aux = (replaceElem c y2 x)
                                                                      in (Table a b (replaceElem aux y1 y) )


move :: Table -> Position -> Position -> Maybe Table
move table (P (i1, j1)) (P (i2, j2)) = if not (connected (i1, j1) (i2, j2)) then Nothing
                                               else if not (findPiesa (i1, j1) table) then Nothing
                                                    else if (findPiesa (i2, j2) table) then Nothing
                                                        else Just (replace table (i1, j1) (i2, j2))

data EitherWriter a = EW {getValue :: Either String (a, String)}

tell :: String -> EitherWriter ()
tell msg = EW (Right (() , msg))

instance Applicative EitherWriter where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor EitherWriter where              
  fmap f ma = pure f <*> ma 

instance Monad EitherWriter where
    return va = EW (Right ( va , " " ))
    ma >>= f = let (Right (va, log1)) = getValue ma
                   (Right (vb, log2)) = getValue (f va)
                in EW(Right (vb, log1 ++ log2))
    EW (Left verr) >>= _ = EW (Left verr)

playRound :: (EitherWriter Table) -> ((Position, Position), (Position, Position)) -> EitherWriter Table
playRound (EW (Left verr)) _ = (EW (Left verr)) 
playRound aux ((P (a1, a2), P (b1, b2)), (P (c1, c2), P (d1, d2))) = 
    let m1 = move table (P (a1, a2)) (P (b1, b2))
        m2 = move table (P (c1, c2)) (P (d1, d2))
        (Right (table, log)) = getValue aux
    in if m1 == Nothing then (EW (Left "Jucatorul One nu poate misca"))
       else let Just m1' = m1
                m2 = move m1' (P (c1, c2)) (P (d1, d2))
            in if m2 == Nothing then (EW (Left ("Jucatorul Two nu poate misca" ++ show c1 ++ show c2)))
               else let Just t = m2
                    in do
                        tell(log)
                        tell("Jucatorul One a mutat din careul " ++ show a1 ++ " pozitia " ++ show a2 ++ 
                                    " in careul " ++ show b1 ++  " pozitia " ++ show b2 ++ "\n")
                        tell("Jucatorul Two a mutat din careul " ++ show c1 ++ " pozitia " ++ show c2 ++ 
                                    " in careul " ++ show d1 ++  " pozitia " ++ show d2 ++ "\n")
                        return t

playGame :: Table -> [(Position, Position)] -> [(Position, Position)] -> EitherWriter Table
playGame table lista1 lista2 = let finList = (zip lista1 lista2)
                                in foldl playRound (EW (Right (table, ""))) finList


printGame :: EitherWriter Table -> IO ()
printGame ewt = do
    let t = getValue ewt
    case t of
      Left v -> putStrLn v
      Right (t,v) -> putStrLn v

list1, list2 :: [(Position,Position)]
list1 = [(P(1,1),P(1,0)), (P(2,2),P(2,1)), (P(1,3),P(2,3)) ]
list2 = [(P(1,6),P(1,5)), (P(2,3),P(3,3)), (P(1,2),P(1,3)) ]
table1 :: Table
table1 = Table [Empty, One, Two, One, Empty, Empty, Two, One]
               [Two, Empty, One, Two, One, Two, One, Two]
               [Two, Two, One, Empty, Empty, One, Two, One]
test41 = printGame $ playGame table1 list1 list2

list3, list4 :: [(Position,Position)]
list3 = [(P(1,1),P(1,0)), (P(2,2),P(2,4)), (P(1,3),P(2,3)) ]
list4 = [(P(1,6),P(1,5)), (P(2,3),P(3,3)), (P(1,2),P(1,3)) ]

test42 = printGame $ playGame table1 list3 list4



table2 :: Table
table2 = Table [Two, One, Two, One, Empty, Empty, Two, One]
               [Two, Empty, One, Two, One, Two, One, Two]
               [Two, Two, One, Empty, Empty, One, Two, One]

table3 :: Table
table3 = Table [Empty, One, Empty, Empty, Empty, Empty, Two, One]
               [Two, Empty, One, Two, One, Two, One, Two]
               [Two, Empty, One, Empty, Empty, One, Two, One]

table4 :: Table
table4 = Table [Empty,Empty,Two,One,Empty,Empty,Two,One]
               [Two,One,One,Two,One,Two,One,Two]
               [Two,Two,One,Empty,Empty,One,Two,One]


test11 = validTable table1 == True
test12 = validTable table2 == False
test13 = validTable table3 == True
test21 = move table2 (P (1,2)) (P(2,2)) == Nothing
test22 = move table1 (P (1,2)) (P(2,2)) == Nothing
test23 = move table1 (P (1,1)) (P(2,1))
       == Just (Table [Empty,Empty,Two,One,Empty,Empty,Two,One]
                      [Two,One,One,Two,One,Two,One,Two]
                      [Two,Two,One,Empty,Empty,One,Two,One])
                      -- table4
test24 = move table1 (P (3,1)) (P(2,1))
        == Just (Table [Empty,One,Two,One,Empty,Empty,Two,One]
                       [Two,Two,One,Two,One,Two,One,Two]
                       [Two,Empty,One,Empty,Empty,One,Two,One])

zipLists :: [a] -> [b] -> [(a,b)]
zipLists xs ys = go xs ys []
  where
    go [] _ zs = zs
    go _ [] zs = zs
    go (x:xs) (y:ys) zs = go xs ys ((x,y):zs)
