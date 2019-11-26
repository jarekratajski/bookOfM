{-# LANGUAGE InstanceSigs #-}
module CombinMonands10 where


swap :: Monad m => Maybe (m a) -> m (Maybe a)
swap Nothing = return Nothing
swap (Just x) = fmap Just x

newtype Writer w a = Writer { runWriter :: (w, a)}

instance Functor (Writer w ) where
    fmap :: (a->b) ->Writer w a -> Writer w b
    fmap f (Writer (w, x)) = Writer ( w, f x)

instance Monoid w => Applicative (Writer w) where
    pure x = Writer ( mempty,  x)


instance Monoid w => Monad (Writer w) where
    return x = Writer ( mempty,  x)
    -- join (Writer (w1, Writer (w2, x))) = Writer (w1 `mappend` w2, x )
    Writer (w1, x) >>= f = let Writer (w2 , y) = f x in Writer ( w1 `mappend` w2, y)


swapW :: ( Monad m , Monoid w) => Writer w (m a) -> m (Writer w a)
swapW (Writer( w , mx) ) = fmap (\a -> Writer ( w, a )) mx



newtype Listed m  a = Listed { unlisted :: [m a] }

instance Monad m => Monad (Listed m) where
    return a = Listed $ [return a]
    (Listed xs) >>= f = Listed $ do
        x <- xs
        y <- mapM (unlisted . f ) x
        return ( concat y)
        
instance Monad m => Functor (Listed m) where
  fmap f li =  Listed $   fmap f (unlisted li)         