oddList :: [Int] -> [Int]
oddList (x:xs) | odd x = x : oddList xs
                | otherwise = oddList xs
oddList _ = []
-- equivalent to filter odd
