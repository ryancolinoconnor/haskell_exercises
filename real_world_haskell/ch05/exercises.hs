-- 1. Add a "fill" which will add spaces as necessary
-- added to prettify.hs
 
--
---
    --

mnl :: Int -> Int-> [Doc]->(Int,Int)
mnl max_w last (d:ds) = case d of
    Line ->
        let (max_w_next,new_last) = (mnl max_w 0 ds)
        in (max max_w max_w_next,new_last)

    _ -> mnl max_w (last + 1) ds
mnl max_w last _ = (max max_w last,last)
max_and_last:: Doc->(Int,Int)
max_and_last x= mnl 0 0 [x] 

add_spaces :: Doc -> Int -> Doc
add_spaces doc n|n==0 = doc
add_spaces doc n = Concat doc (add_spaces (Char ' ') (n-1))
fill :: Int -> Doc -> Doc
fill n d = let (max_width,last) = (max_and_last d)
            in if (max_width >= n)
               then d
               else 
                add_spaces d (n - last) 


-- 2. add a functionality to tab out "]" and "}"
-- Added in "Prettify.hs" though it's quite hacky and doesn't remove the char if it's te escape
                       

isOpen (x) = x=='[' || x=='{' || x=='('
isClose :: Char -> Bool
isClose (x )= x==']' || x=='}' || x==')'
matchit :: Char-> Char-> Bool
matchit (x) (y) = (x=='[' && y==']')||
                    (x=='(' && y==')')||
                    (x=='{' && y=='}')
stackParens :: Char ->[Char] -> [Char]
stackParens (x) (xs) |isClose (x)=  if (matchit (last xs) x)
                    then init xs
                    else xs

stackParens (x) (xs) |isOpen  x = (xs) ++ [x]
                       

stackParens _ (xs) = xs
