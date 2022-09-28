divBy :: Integral a => a -> [a] -> Maybe [a]
divBy _ [] = return [] 
divBy _ (0:_) = fail "division by zero in divBy" 
divBy numerator (denom:xs) = 
                            do next <- divBy numerator xs
                               return ((numerator `div` denom) :next)
--
-- divBy 50 [1,2,5,8,10]
--
-- divBy 50 [1,2,0,8,10]
--
-- take 5 (divBy 100 [1..])
--
