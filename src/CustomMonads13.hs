{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module CustomMonads13 where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Reader
import qualified Data.Map as   M( Map(..), lookup, insert)

-- final style
class Monad m => TicTacToe m where
    info :: Position -> m ( Maybe Player)
    ttake :: Position -> m (Result)

data Position = Position Int Int deriving (Eq, Ord)
data Player = O | X deriving (Eq, Ord)

type Board = M.Map Position Player
--info :: Position -> TicTacToe (Maybe Player)
--info = undefined

data Result = AlreadyTaken { by :: Player}
                    | NextTurn
                    | GameEnded { winner::Player}

-- take:: Position -> TicTacToe Result
--take = undefined

takeIfNotTaken::(Monad m , TicTacToe m) => Position -> m (Maybe Result)
takeIfNotTaken p = do
                      i <- info p
                      case i of
                          Just _ -> return Nothing
                          Nothing -> Just <$> ttake p


instance TicTacToe (ReaderT Player (StateT Board IO)) where
    info p = M.lookup p <$> get
    ttake p = do 
                    l <- info p
                    case l of 
                        Just p' -> return  AlreadyTaken { by = p'}
                        Nothing -> do 
                                        me  <- ask
                                        modify (M.insert p me)
                                        liftIO  (putStrLn "Your next move")
                                        return NextTurn -- need to check state
                                        
                                        
                    
