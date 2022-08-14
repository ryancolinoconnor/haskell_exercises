--- fails at runtime because the pattern
--- matching is non- exhaustive
--- badExample (x:xs) = x + BadExample xs


--- exhaustive matching
goodExample (x:xs) = x + goodExample xs
goodExample _ = 0
