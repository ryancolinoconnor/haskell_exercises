mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _ = acc


niceSum :: [Integer] ->Integer
niceSum xs = foldl (+) 0 xs

nicerSum = foldl (+) 0
