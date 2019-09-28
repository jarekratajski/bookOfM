module MyState where

newtype MyState s a = MyState{ runState :: s -> (a, s)}


--get:: MyState s s
--get (MyState x) = MyState



instance Functor (MyState s) where
  fmap f (MyState x) = MyState ( \y -> let (pa, ps) = x y in  (f pa, ps))

instance Applicative (MyState s) where
  pure a = MyState ( \x -> (a, x))

instance Monad (MyState s) where
  (MyState x) >>= f  = MyState (\y -> let (pa, ps) = x y in runState (f pa) ps)



statePlay :: IO ()
statePlay = do
        putStrLn $ show $ runState nextState 7
        putStrLn $ show $ runState  (nextState >>= monader) 7
      where
          initialState = MyState (\x -> (1, x+1))
          nextState = fmap (\a -> a*2) initialState
          monader  = (\x -> MyState ( \z -> (show z, z+2 )))


get::MyState a a
get = MyState (\x -> (x,x))

put::b->MyState b ()
put x = MyState ( \_ -> ((), x))

--modify::(a->b)->MyState a c ->MyState b c
