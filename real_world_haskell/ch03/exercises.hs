import Data.List
-- exercise 1:


data List a = Cons a (List a)
            | Nil
            deriving (Show)
convfromList (Cons x xs) = [x] ++ (convfromList xs)
convfromList Nil  = []
-- exercise 2: 
data MaybeTree b = MaybeNode b (Prelude.Maybe (MaybeTree b)) (Prelude.Maybe (MaybeTree b))
 deriving (Show) 

 -- end of chapter
 -- exercise 1:
numElem:: [a] -> Int
numElem (x:xs) = 1 + numElem(xs)
numElem [] = 0

computeMean [] = 0
computeMean x = (sum x) / (fromIntegral $ numElem x)


palinDrome (x:xs) = [x] ++ (palinDrome xs) ++ [x]
palinDrome [] = []

-- learned about reverse
isPalindrome xs = xs == (reverse xs)


-- 6. 
sortLen = sortBy (\ a b -> compare (numElem a) (numElem b))

myintersperse (x:xs) a| (numElem xs) >1 = x ++ a ++ (myintersperse xs a)
myintersperse (x:xs) a |(numElem xs) <=1 = x ++ a ++ head xs
