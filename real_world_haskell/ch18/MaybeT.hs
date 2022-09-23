{-# LANGUAGE  DeriveGeneric,AutoDeriveTypeable,MultiParamTypeClasses, FlexibleInstances,   UndecidableInstances, GeneralizedNewtypeDeriving,AllowAmbiguousTypes #-}

import Control.Monad.Signatures
import GHC.Generics
newtype MaybeT m a = MaybeT {
        runMaybeT :: m (Maybe a)
                } deriving (Generic, Generic1) 

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do 
    unwrapped <- runMaybeT x
    case unwrapped of 
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)

x `altBindMT` f = 
    MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a =  MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing


-- -- this will break due to changes in haskell
-- -- see below for proper implementation
-- -- https://hackage.haskell.org/package/transformers-0.6.0.4/docs/src/Control.Monad.Trans.Maybe.html#line-175
-- -- 
-- --
-- instance (Monad m) => Monad (MaybeT m) where
--     return = returnMT
--     (>>=) = bindMT
--     fail = failMT

instance MonadTrans MaybeT where
    lift m = MaybeT (Just `liftM` m)

instance (MonadIO m) => MonadIO (MaybeT m) where 
    liftIO m  = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
    get = lift get
    put k = lift (put k)

-- ... and so on for MonadReader, MonadWriter
