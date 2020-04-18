{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module FinalOpt13 where

class MonadIStack m where
    pop' :: m Integer
    push' ::  Integer -> m ()



data LastOp = Return | LastPop |  LastPush Integer

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
        f <*> x = do f' <- f
                     f' <$> x

instance (Monad m, MonadIStack m) => MonadIStack (WithContext LastOp m) where
          pop' = C $ \context -> case context of
                                  LastPush n -> return (n, Return)
                                  _   -> (, LastPop) <$> pop'
          push' v = C $ \_ -> (,LastPush v) <$> push' v

optimize ::(Monad m, MonadIStack m) => WithContext LastOp m a -> m a
optimize p = do (x, _ ) <- unC p Return
                return x