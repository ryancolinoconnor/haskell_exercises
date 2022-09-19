import DList (DList)
class Monoid a where
    mempty :: a -- id
    mappend :: a -> a -> a -- binary
