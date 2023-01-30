import Data.Functor
import Data.Maybe

ask :: Reader env env
ask = Reader id

instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma 


instance Monad (Reader env) where
    return x = Reader (const x)
    ma >>= k = Reader f
                where
                f env = let va = runReader ma env
                    in runReader (k va) env

{-

newtype Reader env a = Reader { runReader :: env -> a }

ask :: Reader env env
ask = Reader id


tom :: Reader String String
tom = do
    env <- ask -- g i ve s the envi ronment ( here a S t r i n g )
    return ( env ++ " This is Tom. " )

jerry :: Reader String String
jerry = do
    env <- ask
    return ( env ++ " This is Jerry. " )

tomAndJerry :: Reader String String
tomAndJerry = do
                t <- tom
                j <- jerry
                return ( t ++ "\n" ++ j )

runJerryRun :: String
runJerryRun = runReader tomAndJerry "Who is this ? "
-}

newtype Reader env a = Reader { runReader :: env -> a }

data Prop = Var String | Prop :&: Prop
type Env = [ ( String , Bool ) ]

var :: String -> Reader Env Bool
var x = do
    env <- ask
    return $ fromMaybe False ( lookup x env )

eval :: Prop -> Reader Env Bool
eval ( Var x ) = var x
eval ( p1 :&: p2 ) = do
                    b1 <- eval p1
                    b2 <- eval p2
                    return ( b1 && b2 )


runEval prop env = runReader ( eval prop ) env
aux = (Var "ana" :&: Var "maria") :&: Var "david"
aux2 = [("david", True), ("mar", False), ("maria", True), ("ana", True)]