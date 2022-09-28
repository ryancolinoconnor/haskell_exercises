divBy :: Integral a => a -> [a] -> [Maybe a]
divBy numerator denominators = 
    map worker denominators
    where worker 0 = Nothing
          worker x = Just (numerator `div` x)

-- divBy 50 [1,2,5,8,10]
-- divBy 50 [1,2,0,8,10]
-- take 5 (divBy 100 [1..])


