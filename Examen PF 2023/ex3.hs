newtype ReaderM env a = ReaderM { runReaderM :: env -> Either String a }

instance Functor (ReaderM env) where              
  fmap f ma = pure f <*> ma 

instance Applicative (ReaderM env) where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Monad (ReaderM env) where
    return x = ReaderM (\_ -> (Right x))
    ma >>= k = ReaderM f
            where 
                f env = let va = runReaderM ma env
                    in case va of (Left err) -> (Left err)
                                  (Right v) -> runReaderM (k v) env
                    
