data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala soi _) = soi `elem` ["Tarocco", "Moro", "Sanguinello"] 
ePortocalaDeSicilia (Mar _ _) = False


test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l = sum (map (\(Portocala _ n) -> n) (filter ePortocalaDeSicilia l))

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

eMar :: Fruct -> Bool
eMar (Portocala _ _) = False
eMar (Mar _ c) = c

nrMereViermi :: [Fruct] -> Int
nrMereViermi l = length (filter eMar l)

test_nrMereViermi = nrMereViermi listaFructe == 2

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste(Pisica _) = "Meow!"
vorbeste(Caine _ _) = "Woof!"

rasa :: Animal -> Maybe String
rasa(Pisica _) = Nothing
rasa(Caine _ r) = Just r

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

verifica :: Matrice -> Int -> Bool
verifica (M m) x = foldr (\ (L a) b -> b && (sum a) == x) True m

test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True

findneg :: [Int] -> Bool
findneg l = length(filter (<0) l) == 0

doarPozN :: Matrice -> Int -> Bool
doarPozN (M m) x = foldr (\ (L a) b -> b && (findneg a || ((length a) /= x))) True m

testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True
testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False

corect :: Matrice -> Bool
corect (M m) =  let l = (map (\ (L a) -> length a) m) 
                    in foldr (\ a b -> b && a == (head l)) True l

testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True
