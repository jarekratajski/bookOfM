{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}
-- from book
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE MonadComprehensions        #-}

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- added by jarek
--{-# LANGUAGE OverlappingInstances #-}

module Tree where
import Control.Monad

data Tree a = Leaf a | Node (Tree a) (Tree a)
instance (Show a) => Show (Tree a) where
  show (Leaf z)   = show z
  show (Node a b) = "[" ++ (show a) ++","++ (show b)++"]"


relabel :: Tree a -> Int -> (Tree  (Int, a), Int)
relabel (Leaf l) x = ( Leaf (x, l), x+1)
relabel (Node l r) i = ((Node  l1 r1 ), i2)
      where
        (l1,i1) = relabel l i
        (r1, i2) = relabel r i1


-- type WithCounter a = Int -> (a, Int)

-- Exercise 1.1
type State s a = s -> (a, s)

newtype WithCounter a = WithCounter ( State Int a)

next:: WithCounter a -> (a -> WithCounter b ) -> WithCounter b
(WithCounter f)  `next`  g =
          WithCounter (\x ->
          let
            (ra, ri)  =  f x
            (WithCounter z) = g ra
          in z ri)





purez:: a -> WithCounter a
purez v = WithCounter (\x -> (v, x))


instance Functor WithCounter where
      fmap f a = a `next`  (purez f)

instance  Applicative WithCounter  where
  pure = purez


instance  Monad WithCounter  where
  return = purez
  (>>=) = next


relabelMTree:: Tree a -> WithCounter (Tree (a,Int) )
relabelMTree (Leaf l) = WithCounter (\x -> (Leaf (l,x), x+1))
relabelMTree (Node l r ) = (relabelMTree l)  >>= (\w -> relabelMTree r
                      `next` (\z -> purez (Node w z)) )


--
--instance (Show a) => Show WithCounter a
--  where

testTree = Node (Node  (Leaf "x") (Leaf "y") ) (Leaf "z")
