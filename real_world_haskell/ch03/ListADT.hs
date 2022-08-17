--- works like Cons 0
--- since Nil is a list type
data List a = Cons a (List a)
  | Nil 
  deriving (Show)


fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil
-- exercise 1: 
convfromList (Cons x xs) = [x] ++ (convfromList xs)
convfromList Nil  = []

