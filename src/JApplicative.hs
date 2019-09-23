{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module JApplicative where

jap :: Monad m => m (b -> c) -> m b -> m c
jap  mf mb  = mb >>= (\x  ->  ( mf >>= (\y -> return $ y x)) )

class Monad f => JApplicative f where  --needed for Exercise 3.2
    jpure :: a -> f a
    (<@>) :: f (a->b) -> f a -> f b


jfmap :: JApplicative f => (a -> b) -> f a -> f b
jfmap f a =  jap  (jpure f)  a


newtype ZipList a = ZipList  { getZipList :: [a] }

instance Functor ZipList where
    fmap f a = ZipList {  getZipList = fmap f $ getZipList a  } --exercise 3.3



instance Show a =>  Show (ZipList a)  where
    show l  = show $ getZipList l



-- exercise 3.5

class Functor f => Monoidal f where
    unit :: f ()
    (**) :: f a -> f b -> f (a,b)

instance (Applicative f , Functor f) => Monoidal f where
    unit = pure ()
    (**) fa fb = (fmap (\a -> (\x -> (a, x)  )) fa) <*>  fb
    -- fmap (\a -> fmap (\b -> (a,b) ) fb  ) fa
