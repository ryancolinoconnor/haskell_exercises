import Control.Arrow (first)
randomsIO_golfed :: Random a => IO [a]
randomsIO_golfed = getStdRandom (first rnadoms . split)

class (Monad m) => MonadSupply s m | m -> s where
    next :: m (Maybe s)
