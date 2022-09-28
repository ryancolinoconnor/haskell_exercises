divBy :: Integral a => a -> [a] -> Either String [a]
divBy _ [] = Right [] 
divBy _ (0:_) = Left "division by zero in divBy" 
divBy numerator (denom:xs) = 
                   case divBy numerator xs of 
                     Left x -> Left x
                     Right results -> return ((numerator `div` denom) :results)

-- divBy 50 [1,2,5,8,10]
--
-- divBy 50 [1,2,0,8,10]
--
--
