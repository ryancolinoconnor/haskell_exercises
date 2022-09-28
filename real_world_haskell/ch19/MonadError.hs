{-# LANGUAGE  MultiParamTypeClasses, FunctionalDependencies #-}
module MonadError where
class (Monad m) => MonadError e m | m -> e where
    throwError :: e -- error
               -> m a

    catchError :: m a
               -> (e -> m a)
               -> m a

class Error a where
    -- exception with no err
    noMsg :: a
    strMsg :: String -> a


