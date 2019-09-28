module MyState where

import Data.Functor.Contravariant

newtype MyState s a = MyState{ runState :: s -> (a, s)}


--get:: MyState s s
--get (MyState x) = MyState



instance Functor (MyState s) where
  fmap f (MyState x) = MyState ( \y -> let (pa, ps) = x y in  (f pa, ps))

instance Applicative (MyState s) where
  pure a = MyState ( \x -> (a, x))

instance Monad (MyState s) where
  (MyState x) >>= f  = MyState (\y -> let (pa, ps) = x y in runState (f pa) ps)



getSetPlay :: IO()
getSetPlay = do
            putStrLn "what now"
            putStrLn $ show $ runState  monadicAction 4
            where
                monadicAction = do
                            put 7
                            modify (\x->x*2)
                            modify (\x->x+1)
                            get



statePlay :: IO ()
statePlay = do
        putStrLn $ show $ runState nextState 7
        putStrLn $ show $ runState  (nextState >>= monader) 7
        getSetPlay
        contraPlay
      where
          initialState = MyState (\x -> (1, x+1))
          nextState = fmap (\a -> a*2) initialState
          monader  = (\x -> MyState ( \z -> (show z, z+2 )))



get::MyState a a
get = MyState (\x -> (x,x))

put::b->MyState b ()
put x = MyState ( \_ -> ((), x))

modify::(a->a)->MyState a ()
-- modify f = MyState ( \y -> ((), f y))
modify f = do
                i <- get
                put $ f i


-- 6.3modify

newtype Returns r a = R ( a->r)

freturn :: Returns r a -> a -> r
freturn (R x) a= x a


instance  Contravariant (Returns r ) where
    contramap f (R x) = R (  x . f )


contraPlay :: IO ()
contraPlay =
            let
                a = R ( \x -> show $ -x )
                b = contramap  fst a
            in putStrLn $ freturn b (4,5)


