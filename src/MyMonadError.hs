{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module MyMonadError where



class (Monad m) => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
  throwError z = Left z
  catchError (Left z) f = f (z)
  catchError m _ = m


instance MonadError () (Maybe) where
  throwError () = Nothing
  catchError  (Just a) f= Just a
  catchError Nothing f = f()
