module Utilties where

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM  f a b   = sequence $ zipWith f a b

replicateM :: Monad m => Int -> m a -> m [a]
replicateM x a = sequence $ replicate x a


-- filterM :: Monad  m => (a -> m Bool) -> [a] -> [b]
-- filterM p a = sequence $ filter p a