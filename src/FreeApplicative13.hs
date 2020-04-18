{-# LANGUAGE GADTs #-}
module FreeApplicative13 where

data Ap f a where
    Pure :: a -> Ap f a
    Ap :: f a -> Ap f (a -> b) -> Ap f b

instance Functor f => Functor (Ap f) where
    fmap f (Pure a) = Pure (f a)
    fmap f (Ap x y) = Ap x (fmap (f . ) y ) 
   
   
instance Functor f => Applicative (Ap f) where
    pure = Pure
    Pure f <*>  y = fmap f y
    Ap x y <*> z = Ap x (flip <$> y <*> z)    


