(++) :: [a] -> [a] -> [a]
(x:xs) ++ ys = x : xs Prelude.++ ys
_      ++ ys = ys
