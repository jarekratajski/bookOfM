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


type FSError = IOError

data  FS a  where
    WriteFile:: FilePath-> String -> FS (Either FSError ())
    ReadFile :: FilePath ->FS (Either FSError String)
    FSDone :: a -> FS a
    FSBind:: FS a -> (a -> FS b) -> FS b

instance Functor FS where
    fmap  f x = FSBind x (FSDone . f)

instance Applicative FS where
  pure = FSDone

instance Monad FS where
   return = pure
   (>>=) = FSBind

writeFileFS :: FilePath -> String -> FS (Either FSError ())
writeFileFS path contents = WriteFile path contents

readFileFS :: FilePath -> FS (Either FSError String)
readFileFS  path  = ReadFile path


data Program instr a where
      PDone :: a -> Program instr a
      PBind :: Program instr b -> (b-> Program instr a)-> Program instr a
      Instr:: instr a -> Program instr a
      
      
instance Functor (Program instr) where
      fmap f x =  PBind x (PDone . f)
      
      
instance Applicative (Program instr) where
      pure = PDone
      
instance Monad (Program instr) where
      return = pure
      (>>=) = PBind