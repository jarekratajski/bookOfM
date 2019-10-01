module MyTher where


data MyTher e r = MLeft e | MRight r

instance Functor (MyTher e) where
    fmap f (MLeft x) = MLeft x
    fmap f (MRight x) = MRight (f x)

instance Applicative (MyTher e) where
    pure = MRight


instance Monad (MyTher e) where
   --return = MRight
    (MLeft m) >>= n = MLeft m
    (MRight m) >>= n = n m
