{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ApplicativeDo #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module FreeMonad13 where

import qualified Data.Map as   M( Map(..), lookup, insert)
import qualified System.IO as System.IO
import Control.Exception

data TicTacToeF r = Info Position (Maybe Player -> r)
                               | Take Position (Result -> r)


data Position = Position Int Int deriving (Eq, Ord)
data Player = O | X deriving (Eq, Ord)

type Board = M.Map Position Player

data Result = AlreadyTaken { bya :: Player}
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
--Free (Info p  ((fmap f) . (\r -> k r))  ) ??

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap return


foldFree :: Monad m => (forall r. f r -> m r) -> Free f a -> m a
foldFree _ (Pure x) = return x
foldFree interpret (Free x) = do
                                x' <- interpret x
                                foldFree interpret x'


-- fs

type FSError = IOError

data  FSF r = WriteFile FilePath String (Either FSError () -> FSF r )
                  | ReadFile FilePath (Either FSError String -> FSF r)

-- this is not really as in the book
runFSF::FSF a ->  IO a
runFSF  (WriteFile path contents k)  = do
        System.IO.writeFile path contents
        runFSF $ k (Right ())
        `catch`  \ex ->  runFSF $ k (Left ex)

runFSF (ReadFile path k) = do
        contents <- System.IO.readFile path
        runFSF  $ k $ Right contents
        `catch`  \ex -> runFSF $ k (Left ex)

runFS = foldFree runFSF

