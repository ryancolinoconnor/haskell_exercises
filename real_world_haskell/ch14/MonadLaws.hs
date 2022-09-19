fmap id == id
fmap (f . g) == fmap f . fmap g
-- law 1 ret is a left id for >>=
return x >>= f == f x

-- law 2 it is also a right id for >>=
m >>= return === m

-- law 3 given a chain of actions, no matter the syntax the actions should be preserved
m >>= (\x -> f x >>= g) === (m >>= f) >>= g


