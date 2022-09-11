module Prettify2 where 
data Doc = Empty
            | Char Char
            | Text String
            | Line
            | Concat Doc Doc
            | Union Doc Doc
            deriving (Show,Eq)

empty :: Doc
empty = Empty
(<>) :: Doc -> Doc -> Doc
Empty <> y = y
fold:: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty 
hcat :: [Doc] -> Doc
hcat = fold (Prettify2.<>)
char :: Char-> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line
