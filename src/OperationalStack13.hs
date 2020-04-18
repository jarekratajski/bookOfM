{-# LANGUAGE GADTs #-}
module OperationalStack13 where

import Control.Monad.State.Lazy (State,get,put,evalState)

--data TicTacToe a where
--      Info :: Position -> TicTacToe (Maybe Player)
--      Take:: Position -> TicTacToe Result
--      Done :: a -> TicTacToe a
--      Bind:: TicTacToe a -> (a -> TicTacToe b) -> TicTacToe b
--data IStackF r = Pop (Integer -> r) | Push Integer r deriving Functor
--type IStack = Free.Free IStackF
data IStack a where
      Done:: a->IStack a
      Bind :: IStack a -> (a -> IStack b) -> IStack b
      Pop :: IStack  Integer
      Push :: Integer -> IStack ()

instance Functor IStack where
     fmap f x = x >>= (Done . f)

instance Applicative IStack where
      pure = return
      f <*> x =  x >>= (\z -> fmap (\f' -> f' z) f)

instance Monad IStack where
    return = Done
    (>>=) = Bind


optimize :: IStack a -> IStack a
optimize (Done x) = Done x
optimize (Bind (Push x) f) = case f () of
                                                    Pop -> Done x
                                                    otherwise -> Bind (Push x) (\z ->optimize $ f ())
optimize x = x

data RPNInstruction = Number Integer | Plus | Times

evaluates :: [RPNInstruction] -> IStack Integer
evaluates [] = Pop
evaluates ((Number n) : r) = Push n >> evaluates r
evaluates (Plus : r)= ((+) <$> Pop <*> Pop ) >>= Push >> evaluates r
evaluates (Times : r) = ((*) <$> Pop <*> Pop)>>= Push >> evaluates r

interpretStack :: IStack a -> State [Integer] a
interpretStack (Done x) = pure x
interpretStack (Pop )= do
                                                      stck <- get
                                                      put $ tail stck
                                                      case stck of
                                                          [] -> error "stack underflow"
                                                          (x:xs) ->  return  x
interpretStack (Push v ) = do
                                                      stck <- get
                                                      put $ v:stck
interpretStack (Bind x f) = interpretStack x >>= interpretStack . f                                           





foo::[RPNInstruction]->Integer
foo x = evalState (interpretStack $ evaluates x) []


foo1Test :: IO ()
foo1Test = print $ foo myInstructions
                where myInstructions = [Number 3, Number 2, Plus, Number 7, Times]

