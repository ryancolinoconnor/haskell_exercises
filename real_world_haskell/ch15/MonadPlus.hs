mzero >>= f == mzero
v >> == mzero
-- standard guard from control.monad
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero


