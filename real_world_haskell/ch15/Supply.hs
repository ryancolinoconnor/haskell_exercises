{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving#-}
module Supply 
    (
    Supply
  , next
  , runSupply
    ) where
import Control.Monad.State
import Control.Monad (Monad)
newtype Supply s a = S (State [s] a)   deriving (Monad, Functor, Applicative)

next :: Supply s (Maybe s)
next = S $ do st <- get
              case st of
                    [] -> return Nothing
                    (x:xs) -> do put xs
                                 return (Just x)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

showTwo :: (Show s) => Supply s String
showTwo = do
    a <- next
    b <- next
    return (show "a: " ++ show a ++ ", b: " ++ show b)
