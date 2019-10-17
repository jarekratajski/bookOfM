module MyTher where
import Control.Applicative

data MyTher e r = MLeft e | MRight r

instance Functor (MyTher e) where
    fmap f (MLeft x) = MLeft x
    fmap f (MRight x) = MRight (f x)

instance Applicative (MyTher e) where
    pure = MRight
    (MRight f) <*> (MRight a) = MRight (f a)
    _ <*> (MLeft e) = MLeft e




instance Monad (MyTher e) where
   --return = MRight
    (MLeft m) >>= n = MLeft m
    (MRight m) >>= n = n m


instance (Show e, Show r) => Show (MyTher e r) where
   show (MLeft e) = "L:" ++ show e
   show (MRight r) = "R:" ++ show r


-- 7.2
instance (Monoid e) =>Alternative (MyTher e) where
   empty = MLeft mempty
   (MRight a) <|> _ = MRight a
   _ <|> (MRight b) = MRight b


mytherPlay::IO ()
mytherPlay = do
                      putStrLn $ show mther
                      where
                        mther = pure 7 :: MyTher Int Int