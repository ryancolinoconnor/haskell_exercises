import Test.QuickCheck ((==>), Property)
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter (< x) xs
          rhs = filter (>=x) xs
head :: [a] -> a
head (x:_) = x
head [] = error "Prelude.head: empty list"

minimum :: (Ord a) => [a] -> a
minimum [] = error "Prelude.minimum: empty list"
minimum xs = foldl1 min xs

prop_minimum' xs = not (null xs) ==> Main.head (qsort xs) == Main.minimum xs
