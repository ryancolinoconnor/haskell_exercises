
foldl :: (a->b ->a) -> a -> [b] -> a
foldl step zero (x:xs) = Main.foldl step (step zero x) xs
foldl _ zero [] = zero

foldlSum xs = Main.foldl step 0 xs
    where step acc x = acc +xs
niceSum xs = Main.foldl (+) 0 xs

foldr step zero (x:xs) = step x (Main.foldr step zero xs)
foldr _ zero [] = zero

myFilter p xs = Main.foldr step [] xs
    where step x ys | p x = x:ys
                    |otherwise = ys

myMap :: (a->b) ->[a] ->[b]
myMap f xs = Main.foldr step [] xs
        where step x ys = f x :ys


myFoldl :: (a-> b-> a) -> a -> [b] -> a
myFoldl f z xs = Main.foldr step id xs z
            where step x g a = g (f a x)

identity :: [a] -> [a]
identity xs = Main.foldr (:) [] xs

append xs ys = Main.foldr (:) ys xs

foldl' _ zero [] = zero
fold' step zero (x:xs) = 
    let new = step zero x
     in new `seq` foldl step new xs`

-- incorrect usage, seq shouldn't be hidden
hiddenInside x y = someFunc (x `seq` y)
-- incorrect: seame as above
hiddenByLeft x y z = let a = x `seq` someFunc y
    in anotherFunc a z
-- correct usage
onTheOutside x y = x `seq` someFunc y

chained x y z = x `seq` y `seq` someFunc z
badExpression step zero (x:xs) = 
    seq (step zero x)
    (badExpression step (step zero x) xs)

strictPair (a,b) = a `seq` b `seq` (a,b)
strictList (x:xs) = x `seq` x : strictList xs
strictList [] = []
