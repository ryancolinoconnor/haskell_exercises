{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving#-}
module Supplyinstance where
import Control.Monad.Fail (MonadFail)
newtype Reader e a = R {runReader :: e-> a} deriving (Applicative, Functor)

instance Monad (Reader e) where
    return a = R $ \_ -> a
    m >>= k = R $ \r -> runReader (k (runReader m r)) r

ask :: Reader e e
ask = R id


