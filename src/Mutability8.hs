module Mutability8 where

import Control.Monad.ST
import Data.STRef
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.List
import Data.Maybe

weirdSum = runST $
                    do
                    x <- newSTRef  1
                    y <- newSTRef  1
                    modifySTRef y (+1)
                    (+) <$> readSTRef x <*> readSTRef y



addName :: TVar Integer -> TVar [ (Integer, String)] -> String -> STM ()
addName counter names name = do
                                                        i <- readTVar counter
                                                        modifyTVar names ((i, name) :)  -- was bug in the book
                                                        writeTVar counter (i+1)


addName2 :: TVar Integer -> TVar [ (Integer, String)] -> String -> STM ()
addName2 counter names name = do
                                                        i <- readTVar counter
                                                        namesP <- readTVar names
                                                        if (isJust $ find (\x -> x == name) (snd <$> namesP))
                                                            then
                                                                return ()
                                                            else
                                                                do
                                                                modifyTVar names ((i, name) :)
                                                                writeTVar counter (i+1)



mutaplay :: IO ()
mutaplay  = do
                    cnt <- newTVarIO 0
                    pairs <- newTVarIO ([]::[(Integer, String)])
                    atomically $ do
                            addName2 cnt pairs "Irreg"
                            addName2 cnt pairs "Irreg"
                            addName2 cnt pairs "Zyubi"
                    x <- readTVarIO cnt
                    putStrLn $ show x
                    readTVarIO pairs >>= putStrLn . show
                    return ()

