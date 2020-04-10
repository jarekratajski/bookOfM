{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module InitialStyle13 where

import qualified Data.Map as   M( Map(..), lookup, insert)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State.Lazy (StateT, get, modify, State, put)

import qualified System.IO as System.IO
import Control.Exception
import qualified Data.Map as   M( Map(..), lookup, insert)

data TicTacToe a = Info Position (Maybe Player -> TicTacToe a)
                               | Take Position (Result -> TicTacToe a)
                               | Done a


data Position = Position Int Int deriving (Eq, Ord)
data Player = O | X deriving (Eq, Ord)

type Board = M.Map Position Player

data Result = AlreadyTaken { by :: Player}
                    | NextTurn
                    | GameEnded { winner::Player}
--13.6
instance Functor TicTacToe where
   fmap f ( Done a) = Done $ f a
   fmap f (Info p k) = Info p (\r -> fmap f (k r) )
   fmap f (Take p k) = Take p (\r -> fmap f (k r) )

instance Applicative TicTacToe where
    pure = Done

instance Monad TicTacToe where
     return = Done
     (Done x) >>= f = f x
     (Info p k) >>=f  = Info p (\pl -> k pl >>= f)
     (Take p k) >>=f  = Take p (\r -> k r >>= f)


runGame::TicTacToe a -> ReaderT Player (StateT Board IO) a
runGame (Done x) = return x
runGame (Info p k) = do
                                   pl <- M.lookup p <$> get
                                   runGame (k pl)
runGame (Take p k) = do
                                     pl <- M.lookup p <$> get
                                     case pl of
                                          Just p' -> runGame (k $ AlreadyTaken { by = p'})
                                          Nothing -> do
                                                    me <- ask
                                                    modify (M.insert p me)
                                                    runGame ( k NextTurn)

-- exercise 13.5
from::(()->a) -> a
from f = f ()


to::a-> (() -> a)
to x () = x


--
type FSError = IOError

data  FS a = WriteFile FilePath String (Either FSError () -> FS a )
                  | ReadFile FilePath (Either FSError String -> FS a)
                  | FSDone a

writeFileFS :: FilePath -> String -> FS (Either FSError ())
writeFileFS path contents = WriteFile path contents FSDone

readFileFS :: FilePath -> FS (Either FSError String)
readFileFS  path  = ReadFile path FSDone


-- 13.7 ??
--testFSInitial :: IO ()
--testFSInitial = do
--                        result <- writeFileFS "nic" "niewiele"


interpret :: FS a -> IO a
interpret (FSDone x ) = return x
interpret  (WriteFile path contents k)  = do
        System.IO.writeFile path contents
        interpret (k (Right ()))
        `catch` \ex -> interpret (k (Left ex))

interpret (ReadFile path k) = do
        contents <- System.IO.readFile path
        interpret (k  (Right contents))
        `catch`  \ex -> interpret (k (Left ex))

type MockFileSystem = M.Map FilePath String

interpretS:: FS a -> (State MockFileSystem) a
interpretS (FSDone x ) = return x
interpretS (ReadFile path k) = do
                                fs <- get
                                interpretS (k ( maybe nofile  Right $ M.lookup path fs) )
                                where
                                    nofile = Left $ userError "File does not exist"

interpretS (WriteFile path contents k) = do
                              fs <- get
                              put $ M.insert path contents fs
                              interpretS (k (Right ()))

