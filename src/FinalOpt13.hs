{-# LANGUAGE FlexibleInstances #-}
module FinalOpt13 where


data LastOp = Return | LastOp |  LastPush Integer 

newtype WithContext c m a = C { unC:: c -> m (a, c) }

instance Monad m => Monad (WithContext LastOp m) where 
            return x = C $ \_ -> return (x, Return) 
            C x >>= f = C $ \context -> do 
                              (x' , context') <- x context 
                              unC (f x') context'

instance Monad  m => Functor (WithContext LastOp m) where
        fmap f x = x >>= (\z -> return $ f z)

instance Monad m => Applicative (WithContext LastOp m ) where
        pure = return
        f <*> x = do 
                           f' <- f
                           x' <- x
                           return (f' x')                     
                           