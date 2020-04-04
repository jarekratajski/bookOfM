{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Croco13 where
import Control.Monad.State.Lazy
import Control.Monad.Trans.Reader
import Debug.Trace
-- 13.2 - croco

class Monad m => Croco m where
    sayNumber::Int ->m Result
    expectsNumber:: m Bool


data Result = YouWon | YouLost deriving  (Show)

data Winner = First | Second deriving  (Show)

data CrocoState  = Initial | Number Int |  End Winner deriving  (Show)


-- 13.3
-- try numbero uno
instance Croco ( ReaderT Int (State CrocoState) ) where
    sayNumber x =  do
                               state <- get
                               case state of
                                      Initial -> do
                                                     put $ Number x
                                                     ReaderT $ \y -> trace ( "initial x = " ++ show x ++ " y= " ++ show y ) $ if (x < y) then return YouLost else return YouWon
                                      Number y -> let menda = do
                                                      prev <- get
                                                      ReaderT $ \s -> trace ( "number x = " ++ show x ++ " y= " ++ show y  ++ " reader ="++ show s) $ if (x < y) then return YouLost else return YouWon
                                                      in local (\_ -> x ) menda 
                                      End   w -> return YouWon


--                                      Number y ->  do
--                                                      prev <- get
--                                                      ReaderT $ \s -> trace ( "number x = " ++ show x ++ " y= " ++ show y  ++ " reader ="++ show s) $ if (x < y) then return YouLost else return YouWon

    expectsNumber = do
                                  state <- get
                                  case state of
                                        Initial -> return True
                                        otherwise -> return False

initialCroco::ReaderT Int (State CrocoState) ()
initialCroco = ReaderT $ (\_ -> put Initial)

testCroco :: IO ()
testCroco = print $ (evalState $ runReaderT (step1 >> step2 ) 1) Initial
                    where
                          step1 =   sayNumber 4 :: ReaderT Int (State CrocoState) Result
                          step2 =   sayNumber 7 :: ReaderT Int (State CrocoState) Result

