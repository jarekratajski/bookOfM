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
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy

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
  pure a = Evaluator $ ReaderT {runReaderT  =  \_ -> Identity $ Just a }


instance Monad Evaluator where
        return  = pure
        (Evaluator x) >>= f  = Evaluator $ ReaderT  { runReaderT =  \a ->
            case runReader x a of
                Nothing -> Identity Nothing
                Just y    -> let Evaluator e = f y
                                  in runReaderT e a
            }

eval::Expr -> Evaluator Integer
eval (Literal n ) = return n
eval (Var v) = do
                       a <- Evaluator $ fmap Just $ ask
                       case lookup v a of
                          Nothing -> evalFail
                          Just v' ->  return v'
eval (Op o x y) = do
                            u <- eval x
                            v <-eval y
                            case o of
                                Add -> return (u + v)
                                Subtract -> return (u - v)
                                Multiply -> return (u * v)
                                Divide -> if  v==0 then evalFail
                                                              else return (u `div` v)

evalFail :: Evaluator a
evalFail = Evaluator $ ReaderT { runReaderT = \_ -> Identity Nothing}



eval7 :: Expr -> MaybeT (State Assignment) Integer
eval7 (Literal n ) = return n
eval7 (Var v) = do
                              a <-   get
                              case lookup v a of
                                  Nothing -> MaybeT { runMaybeT = state (\z -> (Nothing,z)) }
                                  Just x -> return x
                                 
eval7 (Op o x y) = do
                            u <- eval7 x
                            v <-eval7 y
                            case o of
                                Add -> return (u + v)
                                Subtract -> return (u - v)
                                Multiply -> return (u * v)
                                Divide -> if  v==0 then MaybeT { runMaybeT = state (\a -> (Nothing,a)) }
                                                              else return (u `div` v)
                                                              
                                                              
evaltransformtest::IO ()
evaltransformtest =  putStrLn $ show result
                    where 
                        literal7 =  Literal 7
                        assignment = [("x", 4)]
                        computation = (runState $ runMaybeT  $ eval7 literal7) assignment 
                        result = show  <$> fst computation                                                                                                                     