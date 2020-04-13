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

module Streams13 where

import qualified Data.Map as   M( Map(..), lookup, insert)
import qualified System.IO as System.IO

import Control.Exception


data TicTacToe a where
      Info :: Position -> TicTacToe (Maybe Player)
      Take:: Position -> TicTacToe Result
      Done :: a -> TicTacToe a
      Bind:: TicTacToe a -> (a -> TicTacToe b) -> TicTacToe b


instance Functor TicTacToe where
      fmap f x = Bind x ( Done . f)

instance Applicative TicTacToe where
      pure = Done
      f <*> x = do 
                      x' <- x
                      f' <- f
                      return (f' x')

instance Monad TicTacToe where
      return = Done
      (>>=) = Bind


data Position = Position Int Int deriving (Eq, Ord)
data Player = O | X deriving (Eq, Ord)

type Board = M.Map Position Player

data Result = AlreadyTaken { bya :: Player}
                    | NextTurn
                    | GameEnded { winner::Player}
