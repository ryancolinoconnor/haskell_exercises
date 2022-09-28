divBy :: Integral a => a-> [a] -> Maybe [a]
divBy _ [] =  Just []
divBy _ (0:_) = Nothing
divBy numerator (denom:xs) = case divBy numerator xs of
                               Nothing -> Nothing
                               Just results -> Just ((numerator `div` denom):results)


-- divBy 50 [1,2,5,8,10]

-- divBy 50 [1,2,0,8,10]

