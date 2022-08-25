module Prettify where
punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d Prettify.<> p) : punctuate p ds

data Doc = Empty
    | Char Char
    | Text String
    | Line
    | Concat Doc Doc
    | Union Doc Doc
    deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char-> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (Prettify.<>)


fold :: (Doc->Doc->Doc) ->[Doc] ->Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (Prettify.</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x Prettify.<> softline Prettify.<> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) = case d of
                               Empty -> transform ds
                               Char c -> c : transform ds
                               Text s -> s ++ transform ds
                               Line -> '\n' : transform ds
                               a `Concat` b -> transform (a:b:ds)
                               _ `Union` b -> transform (b:ds)


pretty :: Int-> Doc ->String
pretty width x = best 0 [x]
    where 
        best col (d:ds) = case d of
            Empty -> best col ds
            Char c -> c: (best (col + 1) ds)
            Text s -> s ++ (best (col + length s) ds)
            Line ->'\n':(best 0 ds) 
            a `Concat` b -> best col (a:b:ds)
            a `Union` b -> nicest col (best col (a:ds))
                                    (best col (b:ds))
        best _ _  = ""
        nicest col a b| ((width)- least) `fits` a = a
                  | otherwise = b
                where least = (min width col)
                          --
-- hacky workaround to add in the way of stacking parens
pretty2 :: Int -> Doc -> String
pretty2 width x = (best 0 "" [(fill width x)])
    where
        best col stack (d:ds) = 
            case d of
            Empty -> best col stack ds
            Char c ->
                let new_stack = stackParens c stack in
                c : best (col +1) new_stack ds
            Text s -> s ++ best (col + length s) stack ds
            Line ->( 
                case (head ds) of
                Char c -> let new_stack = stackParens c stack in
                    ['\n']++(((length new_stack)*4) `replicate` ' ') ++ (best (length new_stack) stack ds)
                _ ->['\n']++(((length stack)*4) `replicate` ' ') ++ (best (length stack) stack ds))
            a `Concat` b -> best col stack (a:b:ds)
            a `Union` b -> nicest col (best col stack (a:ds))
                                    (best col stack (b:ds))
        best _ _ _ = ""

        nicest col a b| ((width)- least) `fits` a = a
                  | otherwise = b
                    where least = (min width col)


fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n':_) = True
w `fits` (c:cs) = (w - 1) `fits` cs

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
