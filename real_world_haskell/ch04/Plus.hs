-- infix
a `plus` b = a + b

data a `Pair` b = a `Pair` b
    deriving (Show)
-- foo fighters would hate this
foo = Pair 1 2
bar = True `Pair` "quux"

