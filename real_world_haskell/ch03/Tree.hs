data Tree a = Node a (Tree a) (Tree a)
  | Empty
  deriving (Show)


data Maybe a = Just a
  | Nothing
data MaybeTree b = MaybeNode b (Prelude.Maybe (MaybeTree b)) (Prelude.Maybe (MaybeTree b))
 deriving (Show)  
