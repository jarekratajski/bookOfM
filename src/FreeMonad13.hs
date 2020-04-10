module FreeMonad13 where

import qualified Data.Map as   M( Map(..), lookup, insert)

data TicTacToeF r = Info Position (Maybe Player -> r)
                               | Take Position (Result -> r)


data Position = Position Int Int deriving (Eq, Ord)
data Player = O | X deriving (Eq, Ord)

type Board = M.Map Position Player

data Result = AlreadyTaken { by :: Player}
                    | NextTurn
                    | GameEnded { winner::Player}


instance Functor TicTacToeF where
    fmap f (Info position mpf) = Info position ( f . mpf)
    fmap f (Take position resf) = Take position ( f . resf)

data Free f a = Free ( f (Free f a))
                        | Pure a


instance Functor f => Functor (Free f) where
      fmap f (Pure x) = Pure ( f x)
      fmap f (Free x)= Free (fmap (fmap f) x)

instance Functor f => Applicative (Free f) where
    pure = Pure
    Pure f <*> Pure x = Pure (f x)
    Pure f <*> Free x  = Free ( fmap (fmap f) x)
    Free f <*> x = Free (fmap (<*> x) f)

instance Functor f => Monad (Free f) where
     return = Pure
     Pure x >>= f  = f x
     Free x >>= f = Free (fmap (>>=f) x)

type TicTacToe = Free TicTacToeF


-- 13.10

--fmap f (Free (Info p k))
--Free (fmap (fmap f)  (Info p k))
---- Free (fmap (fmap f) (Free (Info p (\mp -> r)  )))
--Free (Info p  ((fmap f) . k)  )
--Free (Info p  ((fmap f) . \r -> k r)  ) ??

