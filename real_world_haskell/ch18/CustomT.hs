newtype CustomT m a 

instance MonadReader r m => MonadReader r (CustomT m) where
instance MonadIO  m => MonadIO (CustomT m) where


