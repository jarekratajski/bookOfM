module JMonad where

class  JMonad m where
  purev  :: a -> m a
  (>!>=) :: (m a ) -> ( a -> m b ) -> m b
