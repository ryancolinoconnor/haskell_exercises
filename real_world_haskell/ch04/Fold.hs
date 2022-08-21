
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
