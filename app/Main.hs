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

{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ApplicativeDo #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}



module Main where

import Lib
import Tree
import MagicBoxes
import MayTheOption
import JMonad
import JApplicative
import Utilties
import MyState

class MEq a where
  (=~=) :: a -> a -> Bool


instance (MEq Int) where
  a =~= b = a == b

--instance (Num p, Eq p) => MEq  p where
--    a =~= b = a == b


instance (MEq a, MEq b) => MEq (a, b) where
    (x1, y1) =~= (x2, y2) = (x1 =~= x2) && (y1 =~= y2)


notEq::Eq a => a -> a -> Bool
notEq x y  = not $ x == y

testEq = do
    putStrLn "hello"
    putStrLn $ show $ (1::Int) =~= (2::Int)
    putStrLn $ show $ (2::Int) =~= (2::Int)
    putStrLn $ show $ (3::Int,1::Int) =~= (3::Int,2::Int)
    putStrLn $ show $ (3::Int,2::Int) =~= (3::Int,2::Int)
    putStrLn $ show $ notEq 7 5





relabelPlay :: IO ()
relabelPlay = do
              putStrLn $ show $ relabel testTree 1
              putStrLn $ show $ let ( WithCounter g ) =  (relabelMTree testTree) in g 1
              putStrLn $ show $ lx +++ ly
              putStrLn $ show $ map (\x -> x+x) lx -- Exercise 1.3
              putStrLn $ show $ jap (Just (\x -> x + x)) (Just 5)  -- Exercise 3.1
              putStrLn $ show $ fmap (\a -> a-1 ) zipList
         where
            lx = [1,2,5,7,8]
            ly = [2,7,8]
            zipList   = ZipList { getZipList = [1::Int,6,4,1,2] }

main = statePlay