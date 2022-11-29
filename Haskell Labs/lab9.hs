import Data.List (nub)
import Data.Maybe (fromJust)

type Nume = String
data Prop
    = Var Nume
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    | Prop :->: Prop
    | Prop :<->: Prop
    deriving Eq
infixr 2 :|:
infixr 3 :&:
infixr 4 :->:
infixr 5 :<->:


p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R") :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R"))))

instance Show Prop where
    show F = "False"
    show T = "True"
    show (Var n) = n
    show (Not a) = "(~" ++ (show a) ++ ")"
    show (a :|: b) = "(" ++ (show a) ++ "|" ++ (show b) ++ ")"
    show (c :&: d) = "(" ++ (show c) ++ "&" ++ (show d) ++ ")"
    show (c :->: d) = "(" ++ (show c) ++ "->" ++ (show d) ++ ")"
    show (c :<->: d) = "(" ++ (show c) ++ "<->" ++ (show d) ++ ")"


test_ShowProp :: Bool
test_ShowProp =
    show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

type Env = [(Nume, Bool)]
impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval T vals = True
eval F vals = False
eval (Var n) vals = impureLookup n vals
eval (Not a) vals = not (eval a vals)
eval (a :|: b) vals = (eval a vals) || (eval b vals)
eval (c :&: d) vals = (eval c vals) && (eval d vals)
eval (c :->: d) vals = not (eval c vals) || (eval d vals)
eval (c :<->: d) vals = (eval c vals) == (eval d vals)

test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

variabile :: Prop -> [Nume]
variabile T = []
variabile F = []
variabile (Var n) = [n]
variabile (Not a) = nub (variabile a)
variabile (a :|: b) = (nub ((variabile a) ++ (variabile b)))
variabile (c :&: d) = (nub ((variabile c) ++ (variabile d)))
variabile (c :->: d) = (nub ((variabile c) ++ (variabile d)))
variabile (c :<->: d) = (nub ((variabile c) ++ (variabile d)))

test_variabile = variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

envs :: [Nume] -> [Env]
envs [] = [[]]
envs (var:rest) = [(var, False) : rest' | rest' <- envs rest] ++ [(var, True) : rest' | rest' <- envs rest]
test_envs =
    envs ["P", "Q"] ==
    [ [ ("P",False)
    , ("Q",False)
    ]
    , [ ("P",False)
    , ("Q",True)
    ]
    , [ ("P",True)
    , ("Q",False)
    ]
    , [ ("P",True)
    , ("Q",True)
    ]
    ]

satisfiabila :: Prop -> Bool
satisfiabila p =  foldl (||) False [eval p lvar | lvar <- envs (variabile p)]

test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

valida :: Prop -> Bool
valida p =  foldl (&&) True [eval p lvar | lvar <- envs (variabile p)]

test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
test_valida2 = valida (Not (Var "P") :|: Var "P") == True

echivalenta :: Prop -> Prop -> Bool
echivalenta pr1 pr2 = let vars = nub ((variabile pr1) ++ (variabile pr2))
                        in [eval pr1 lvar | lvar <- envs vars] == [eval pr2 lvar | lvar <- envs vars]

test_echivalenta1 =
    True
    ==
    (Var "P" :&: Var "Q") `echivalenta` (Not (Not (Var "P") :|: Not (Var "Q")))

test_echivalenta2 =
    False
    ==
    (Var "P") `echivalenta` (Var "Q")

test_echivalenta3 =
    True
    ==
    (Var "R" :|: Not (Var "R")) `echivalenta` (Var "Q" :|: Not (Var "Q"))