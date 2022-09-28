data DivByError a = DivBy0
                    | ForbiddenDenominator a
                    deriving (Eq, Read, Show)


divBy :: Integral a => a -> [a] -> Either (DivByError a) [a]
divBy _ [] = Right [] 
divBy _ (0:_) = Left DivBy0
divBy _ (10:_) = Left (ForbiddenDenominator 10)
divBy numerator (denom:xs) = 
                   case divBy numerator xs of 
                     Left x -> Left x
                     Right results -> return ((numerator `div` denom) :results)

-- divBy 50 [1,2,5,8,10]
--
-- divBy 50 [1,2,0,8,10]
--
--
