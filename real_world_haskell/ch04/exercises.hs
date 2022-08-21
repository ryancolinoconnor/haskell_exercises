import Data.Typeable
import Data.Char (digitToInt)
import System.Environment (getArgs)
-- 1."safe definitions of standard"
data Maybe a = Just a| Nothing
safeHead :: [a] -> Prelude.Maybe a
safeHead [] = Prelude.Nothing
safeHead xs = Prelude.Just (head xs)
safeTail :: [a] -> Prelude.Maybe [a]
safeTail [] = Prelude.Nothing
safeTail xs = Prelude.Just (tail xs)
safeLast :: [a] -> Prelude.Maybe a
safeLast [] = Prelude.Nothing
safeLast xs = Prelude.Just (last xs)
safeInit :: [a] -> Prelude.Maybe [a]
safeInit [] = Prelude.Nothing
safeInit xs = Prelude.Just (init xs)
-- 2. splitWith similar to words but takes predicate and any type
--
splitWith :: (a-> Bool) -> [a] -> [[a]]
splitWith predicate xs|null xs = []
splitWith predicate xs= let (a,b) = (span predicate xs) in
                if (null a)
                then
                    splitWith predicate (tail b)
                else
                [a] ++ splitWith predicate b
-- 3. Program to print the first word of each input
-- 




interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
-- replace "id" with the name of our function below
--


isLineTerminator c = c == '\r'|| c == '\n'
splitLines :: String -> [String]
splitLines [] = []
splitLines cs = 
    let (pre, suf) = break isLineTerminator cs
     in pre : case suf of 
        ('\r':'\n':rest) -> splitLines rest
        ('\r':rest) -> splitLines rest
        ('\n':rest) -> splitLines rest
        _ ->[]
firstword (line:lines) = if null (words line)
                        then firstword lines
                        else
                        [(head (words line))]++firstword lines
firstword [] = []
fixLines:: String -> String
fixLines input = unlines (firstword (splitLines input))
myFunction = fixLines


-- 4. Transpose a text file
--
transposeLines theselines = transposeFile (head theselines) (tail theselines) [] ""
transposeFile :: ([Char])-> [[Char]]-> [[Char]]-> [Char] -> [Char]
cleanhead [] = []
cleanhead xs = head xs
cleantail [] = []
cleantail xs = tail xs
notnull xs = (null xs) /= True
transposeFile (char:line) theselines the_rest str_run |null line && (notnull theselines)= transposeFile (cleanhead theselines) (cleantail theselines) (the_rest ) (str_run ++ [char])  
transposeFile (char:line) theselines the_rest str_run |null theselines = 
    let this_rest = the_rest ++ [line]
    in
    if (null this_rest)||this_rest==[""]
       then str_run ++[char] ++ "\n"
    else
       transposeFile (head this_rest) (tail this_rest) [] (str_run++ [char] ++"\n")
transposeFile (char:line) theselines the_rest str_run = transposeFile (head theselines) (tail theselines) (the_rest ++ [line]) (str_run ++ [char])
-- transposeLines ["hello","world"]
-- "hw\neo\nlr\nll\od\n"
--
--
-- end of chapter
--
-- 1. use a fold to improve asint 

-- 2. handle dots and whatnot
maxInt = 2147483647
asInt_fold xs = Prelude.foldl step 0 xs
    where step acc  x =  if x=='.' then (error "not an int") 
                                else 
                                let new_acc = acc * 10 + (digitToInt x)
                                 in if new_acc>=maxInt
                                    then
                                    (error "overflow")
                                else
                                new_acc
-- 3. Asint either
type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
-- asInt_either  xs = Prelude.foldl step 0 xs
 --   where step acc  x =  if x=='.' then ErrorMessage ( "not an int")
--                                             else acc * 10 + (Int (digitToInt x))

step (Right acc)  x =  if x=='.' then (Left "not an int") 
                            else
                        let new_acc = (acc * 10 + digitToInt x)
                         in if new_acc>=maxInt
                            then
                            (Left "overflow")
                            else
                                Right new_acc
step other _ = other
isString n = typeOf n == typeOf "foo"
asInt_either x  = Prelude.foldl step (Right 0) x


-- 4. COncat a list of list
myConcat :: [[a]] -> [a]
myConcat xs = Prelude.foldr step [] xs
                where step acc x = acc ++ x
-- 5. Takewhile using recursion then foldr
--
takeWhile_recursive f (x:xs)|f x = x:(takeWhile_recursive f xs)
                            |otherwise = []
takeWhile_foldr f xs = Prelude.foldr step [] xs
    where step x ys | f x = x:ys
                    |otherwise = []
-- 6. groupby using fold
good_tail ys = if (length ys)==1 then [(head ys)] else (tail ys)  
consistent_tail_nested :: [a]->a
consistent_tail_nested ys = head (good_tail ys)
fold_groupBy :: (a-> a->Bool) -> [a] -> [[a]]
fold_groupBy f (x:xs) = Prelude.foldl step [[x]] xs
    where step ys x| f (consistent_tail_nested (consistent_tail_nested ys)) x = (init ys) ++ [((consistent_tail_nested ys)++[x])]
                            |otherwise = ys ++ [[x]]

-- fold_groupBy (==) [1,1,1,2,3,]
-- [[1,1,1],[2],[3]]
--
-- 7. How many prelude can you rewrite using foldls, foldl or foldr
-- any, foldr- easy to just do the first computation and not store a bunch of useless thunks
-- cycle not in a very seamless way, maybe you could using folddr and create a function to step n times
-- words, possible, either way- foldr
-- unlines likely foldr again because don't need to hold on to thunks but I guess one should analyze the memory ovehead and scaling, for another day
