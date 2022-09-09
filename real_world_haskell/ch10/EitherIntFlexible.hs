{-# LANGUAGE FlexibleInstances #-}
--- seems this no longer works because 
-- theres a functor for either in Data
instance Functor (Either Int) where
    fmap _ (Left n) = Left n
    fmap f (Right r) = Right (f r)
