data Maybe a = Just a
  | Nothing

someBool = Prelude.Just True
someString = Prelude.Just "something"


wrapped = Prelude.Just (Prelude.Just "wrapped")
