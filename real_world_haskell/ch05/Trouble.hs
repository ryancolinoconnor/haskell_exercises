

-- bad function that ghc doesn't complain about
uppercaseFirst (c:cs) = toUpper c -- forgot cs and just maps a String
camelCase :: String -> String
camelCase xs = concat (map upcaseFirst (words xs))-- ghc will complain!
