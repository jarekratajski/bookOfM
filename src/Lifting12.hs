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
module Lifting12 where

import Control.Monad.Trans.Reader ( ReaderT(..) )
import qualified Control.Monad.Trans.Reader as RT (ask, local, reader)
import Control.Monad.ST.Lazy.Safe (ST)
import System.IO (IOMode, Handle, withFile)
import Control.Monad.IO.Unlift


class Monad m => MonadReader r m | m -> r where
    ask   :: m r
    ask = reader id

    reader :: (r -> a)
           -> m a
    reader f = do
      r <- ask
      return (f r)

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    -- | @'writer' (a,w)@ embeds a simple writer action.
    writer :: (a,w) -> m a
    writer ~(a, w) = do
      tell w
      return a

    -- | @'tell' w@ is an action that produces the output @w@.
    tell   :: w -> m ()
    -- tell w = writer ((),w)

class (Monad m) => MonadError e m | m -> e where
    throwError :: e -> m a




instance MonadReader r m => MonadReader r (ReaderT r m) where
 ask = RT.ask


instance MonadWriter w m => MonadWriter w  (ReaderT w m) where
    tell x = ReaderT $ (\_ -> tell x)

instance MonadError e m => MonadError e (ReaderT  e m) where
    throwError x = ReaderT $ (\_ -> throwError x)


-- 12.2

class (Monad b, Monad m) =>  MonadBase b m | m -> b where
    liftBase :: b a -> m a


instance  MonadBase IO IO where
    liftBase = id -- ???

instance  MonadBase (ST s) (ST s) where
        liftBase = id -- ???


-- 12.3

myWithFile:: (MonadUnliftIO m ) =>FilePath -> IOMode -> (Handle -> m r ) -> m r
myWithFile path mode handler = withRunInIO $ \run -> withFile path mode (run . handler) 

         
                   
          
