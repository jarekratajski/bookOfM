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

{-# LANGUAGE TupleSections #-}

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
  a <- Evaluator $ asks Just
  case lookup v a of
    Nothing -> evalFail
    Just v' -> return v'
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
  a <- get
  case lookup v a of
    Nothing -> MaybeT {runMaybeT = state (Nothing, )}
    Just x -> return x

eval7 (Op o x y) = do
  u <- eval7 x
  v <- eval7 y
  case o of
    Add -> return (u + v)
    Subtract -> return (u - v)
    Multiply -> return (u * v)
    Divide ->
      if v == 0
        then MaybeT {runMaybeT = state (Nothing, )}
        else return (u `div` v)


evaltransformtest::IO ()
evaltransformtest =  print result
                    where
                        literal7 =  Literal 7
                        assignment = [("x", 4)]
                        computation = (runState $ runMaybeT  $ eval7 literal7) assignment
                        result = show  <$> fst computation



-- monad instances
data MyEitherT e m a = MyEitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (MyEitherT e m) where
     fmap f x = MyEitherT $ fmap (fmap f) (runEitherT x)

instance (Applicative m) => Applicative (MyEitherT e m ) where
    pure x = MyEitherT $ pure ( Right x)

instance (Monad m) => Monad (MyEitherT e m ) where
    return = pure
    ( MyEitherT x )  >>= f = MyEitherT $ do
                                            y <- x
                                            case y of
                                                Right r -> runEitherT $ f r
                                                Left l -> return $ Left l

newtype MyStateT s m a = MyStateT{runMyStateT :: s -> m (a, s)}

unMyStateT::(Monad m) => MyStateT s m a ->s -> m a
unMyStateT sm x =  fmap fst  $ runMyStateT sm $ x


instance (Functor m )=> Functor (MyStateT  s m ) where
      fmap f x = MyStateT  func
              where
                  func y
                    = let result = runMyStateT x y
                          f1 (y1, s1) = (f y1, s1)
                        in fmap f1 result


instance (Applicative m) => Applicative (MyStateT s m) where
      pure x = MyStateT $ \s -> fmap (, s) (pure x)

instance (Monad m) => Monad (MyStateT s m ) where
    return = pure
    (MyStateT x) >>= f = MyStateT $ \s ->
              let
                  calculated = x s -- m ( a, s)
                  justa = fmap fst calculated -- m a
                  extf z = (runMyStateT $ f z) s
                  inInnerMonad = justa >>= extf
              in inInnerMonad



-- testing functor
myMaybeA1 = Just 5

myMaybeStateA1 = pure 5 :: (MyStateT Integer Maybe Integer)
myMaybeNext = fmap (+1) myMaybeStateA1
myMaybeStateA2 = myMaybeNext >>= (\x -> pure $ x+1)

testStateT::IO ()
testStateT = print (unMyStateT myMaybeNext 1) >>
        print (unMyStateT myMaybeStateA2 1) >>
        print "o"


newtype MyReaderT r m a = MyReaderT { runMyReaderT :: r -> m a }

instance (Functor m) => Functor (MyReaderT r m) where
    fmap f x  = MyReaderT $ \r -> fmap f (runMyReaderT x r)

instance (Applicative m) => Applicative (MyReaderT r m) where
      pure x = MyReaderT $ \r -> pure x

instance (Monad m) => Monad (MyReaderT r m ) where
      return = pure
      (MyReaderT x) >>= f = MyReaderT $ \r ->
                    let
                          inmonad = x r
                          innerf z = (runMyReaderT $ f z) r
                          bound = inmonad >>= innerf
                     in  bound

--
data MyWriterT w m a  = MyWriterT { runMyWriterT :: m (a, w) }

instance (Monoid w, Functor m) => Functor (MyWriterT w m) where
      fmap f x =  MyWriterT $ fmap innerf innerMonad
                      where
                          innerMonad = runMyWriterT x
                          innerf (a,w) = (f a, w)

instance (Monoid w, Applicative m) => Applicative (MyWriterT w m ) where
    pure x = MyWriterT  $ pure (x, mempty)

instance (Monoid w, Monad m) => Monad (MyWriterT w m) where
    return  = pure
    (MyWriterT x) >>= f = MyWriterT $
                do
                 (a,w1) <- x
                 (b,w2) <- runMyWriterT $ f a
                 return (b, mappend w1 w2)

tell::(Monad m) => w -> MyWriterT w m ()  
tell v = MyWriterT  $ pure ((), v)


instance Semigroup Integer where
    (<>) = (+)

instance Monoid Integer where
  mempty = 0


myMaybeWriteA1 = pure 5 :: (MyWriterT Integer Maybe Integer)
myMaybeWriteNext = fmap (+1) myMaybeWriteA1
myMaybeWriteA2 = tell 12 >> myMaybeWriteNext >>= (\x ->  tell 2 >> (pure $ x+3)  )

testWriteT::IO ()
testWriteT = print (runMyWriterT myMaybeWriteNext) >>
        print (runMyWriterT myMaybeWriteA2) >>
        print "o writer"

