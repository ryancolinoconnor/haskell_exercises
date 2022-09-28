divBy :: Integral a => a -> [a] -> Maybe [a]
divBy = divByGeneric

divByGeneric :: (Monad m, Integral a) => a -> [a] -> m [a]
divByGeneric _ [] = return [] 
divByGeneric _ (0:_) = fail "division by zero in divByGeneric" 
divByGeneric numerator (denom:xs) = 
                            do next <- divByGeneric numerator xs
                               return ((numerator `div` denom) :next)





-- divBy 50 [1,2,5,8,10]
--
-- (divByGeneric 50 [1,2,0,8,10])::(Integral a => Either String [a])
-- -- still yields the exception due to changes in control.monad
