-- 1.1 Using QuickCheck write a test for an action in the monadhandle monad in order to see if it tried to write to a file handle that is not open, try it out on safehello
import System.Directory (removeFile)
import System.IO (IOMode(..))
import MonadHandleIO
import MonadHandle
import Test.QuickCheck.Monadic
import Test.QuickCheck
import Test.QuickCheck.Modifiers (NonEmptyList (..))
import Data.List.Split
-- hacky workaround to see that the file
prop_written_file :: [Char] -> IO Bool
prop_written_file path = do 
    safeHello path
    sample <- safeRead path
    removeFile path
    let equals = sample == "hello world\n"
    return equals

prop_path_generated :: [Char] -> Property
prop_path_generated path | (path=="")||'/' `elem` path = monadicIO $ do assert (True)
prop_path_generated path = monadicIO $ do
    resolves <- run (prop_written_file ("./" ++ path))
    assert (resolves)

-- 1.2 write an action that tries to write to a file handle that it has closed, does your test catch this bug
-- does your test catch this bug, yes it would because it popped an illegal operation error  unsafeHello "ahh" leads to an exception, though it should technically be templatized to accept a function and expected string output" 
unsafeHello :: MonadHandle h m => FilePath -> m ()
unsafeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "hello world"
    hClose h
    hPutStrLn h "bad text"

-- 1.3 
-- You can use an associationlist with a list as the value
-- hacky workaround, could investigate data.map but the interface was a little annoying
decodeString string delim sep = 
    let output = splitOn delim string 
        in (recurse [] output) 


updateKey k s v list = let (startEnd) = splitWhen (\(x,y)-> x==k) list
    in (head startEnd) ++ [(k, s++[v])] ++ (last startEnd)

addOrInsert :: [Char] -> [Char] -> [([Char],[[Char]])]->[([Char],[[Char]])]
addOrInsert k v list =  case lookup k list of 
                          Just s -> updateKey k s v list
                          Nothing -> list ++ [(k,[v])]

safeSplit :: Char -> [Char] -> [[Char]]
safeSplit  delim searchStr= if delim `elem` searchStr then
                                splitOn [delim] searchStr 
                            else
                                [searchStr,"Empty"]
recurse alist [] = alist 
recurse alist (string:strings)  = let kv = safeSplit '=' string  in
        let alist' = (addOrInsert (head kv) (last kv) alist) in 
            recurse alist' strings

