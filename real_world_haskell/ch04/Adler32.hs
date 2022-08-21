foldl :: (a->b ->a) -> a -> [b] -> a
foldl step zero (x:xs) = foldl step (step zero x) xs
foldl _ zero [] = zero

foldlSum xs = foldl step 0 xs
    where step acc x = acc +xs
niceSum xs = foldl (+) 0 xs

foldr step zero (x:xs) = step x (foldr step zero xs)
foldr _ zero [] = zero
