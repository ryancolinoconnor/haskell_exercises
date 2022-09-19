{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, StandaloneDeriving#-}
module SupplyClass
    (
    MonadSupply(..)
  , S.Supply
  , S.runSupply
    ) where 
import qualified Supply as S
import Control.Monad (Monad,liftM)
import Supplyinstance
class (Monad m) => MonadSupply s m | m -> s where
    next :: m (Maybe s)


instance MonadSupply s (S.Supply s) where
    next = S.next


showTwo_class :: (Show s, Monad m, MonadSupply s m) => m String
showTwo_class = do
    a <- next
    b <- next
    return (show "a: " ++ show a ++ ", b: " ++ show b)
    
newtype MySupply e a = MySupply {runMySupply :: Reader e a}
    deriving (Applicative, Functor, Monad)

instance MonadSupply e (MySupply e) where
    next = MySupply (Just `liftM` ask)

xy :: (Num s, MonadSupply s m) => m s
xy = do
    Just x <- next
    Just y <- next
    return (x* y)

runMS :: MySupply i a -> i -> a
runMS = runReader . runMySupply
