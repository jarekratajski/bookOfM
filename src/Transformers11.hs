{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- from book
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
module Transformers11 where

import Control.Monad.Identity
import Control.Monad.Reader

type Name = String
data Expr =  Literal Integer | Var Name | Op Op Expr  Expr
data Op  = Add | Subtract | Multiply | Divide


type Assignment =[(Name, Integer)]
eval2 :: Expr ->Assignment -> Maybe Integer
eval2 (Literal n) _ = return n
eval2 (Var v) a = lookup v a
eval2  (Op o x y ) a = do
                                 u <- eval2 x a
                                 v <- eval2 y a
                                 case o of
                                      Add -> return ( u + v)
                                      Subtract -> return (u - v)
                                      Multiply -> return (u * v)
                                      Divide -> if  (v == 0) then Nothing
                                                                         else return ( u `div` v)


data Evaluator a  =  Evaluator (Reader Assignment (Maybe a))



instance Functor Evaluator where
    fmap::(a->b)->Evaluator a->Evaluator b
    fmap f (Evaluator x) = Evaluator $ ReaderT  { runReaderT =  \a ->
          case runReader x a of
                Nothing -> Identity Nothing
                Just y -> Identity $ Just $  f y
            }


instance Applicative Evaluator where
  pure a = Evaluator $ Reader $ \_ -> Just a


instance Monad Evaluator where
        return x = Evaluator $ Reader $ \_ -> Just x
        (Evaluator x) >>= f  = Evaluator $ Reader $ \a ->
            case runReader x a of
                Nothing -> Nothing
                Just y    -> let Evaluator e = f y
                                  in runReader e a