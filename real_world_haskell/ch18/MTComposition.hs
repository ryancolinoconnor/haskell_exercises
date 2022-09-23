{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Writer
-- import MaybeT
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

problem :: MonadWriter [String] m => m ()
problem = do
    tell ["this is where I fail"]
    fail "oops"

-- which gives the information
-- type A = WriterT [String] MaybeT

type B = MaybeT (Writer [String])

a :: B ()
a = problem

b :: B ()

b = problem

-- should be noted that the version from A no longer worked would but b works
-- runWriter $ runMaybeT b
      

