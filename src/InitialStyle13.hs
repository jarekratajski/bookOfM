{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module InitialStyle13 where

import qualified Data.Map as   M( Map(..), lookup, insert)

data TicTacToe a = Info Position (Maybe Player -> TicTacToe a)
                               | Take Position (Result -> TicTacToe a)
                               | Done a


data Position = Position Int Int deriving (Eq, Ord)
data Player = O | X deriving (Eq, Ord)

type Board = M.Map Position Player

data Result = AlreadyTaken { by :: Player}
                    | NextTurn
                    | GameEnded { winner::Player}

instance Functor TicTacToe where
   fmap f ( Done a) = Done $ f a
   fmap f (Info p k) = Info p (\r -> fmap f (k r) )
   fmap f (Take p k) = Take p (\r -> fmap f (k r) )

instance Applicative TicTacToe where
    pure = Done

instance Monad TicTacToe where
     return = Done
     (Done x) >>= f = f x
     (Info p k) >>=f  = Info p (\pl -> k pl >>= f)
     (Take p k) >>=f  = Take p (\r -> k r >>= f)

-- exercise 13.5
from::(()->a) -> a
from f = f ()


to::a-> (() -> a)
to x () = x




