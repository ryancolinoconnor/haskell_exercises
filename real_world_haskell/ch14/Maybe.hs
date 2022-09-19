data Maybe a = Nothing
            | Just a

--inject :: a -> m a

class Monad m where
    -- chain
    (>>=) :: m a -> ( a -> m b) -> m b
    -- inject
    return :: a -> m a
    fail :: String -> m a
    fail = error
    (>>) :: m a -> m b -> m b
    a >> f = a Main.>>= \_ -> f

instance Monad Maybe where
    Just x >>= k = k x
    Nothing >>= _ = Nothing
    return x = Just x
    fail _ = Nothing

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing = n
maybe _ f (Just x) = f x
