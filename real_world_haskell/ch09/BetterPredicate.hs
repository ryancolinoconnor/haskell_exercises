import Control.Monad (filterM)
import System.Directory (Permissions(..),getModificationTime, getPermissions)
--import System.Time (ClockTime(..))
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Control.Exception (SomeException)
import RecursiveContents (getRecursiveContents)
handler :: SomeException -> IO (Maybe Integer)
handler _ = return Nothing
type Predicate = FilePath -- path to directory entry
                -> Permissions -- Permissions
            -> Maybe Integer -- file size (nothing if not file)
            -> UTCTime -- last modified
            -> Bool

type InfoP a = FilePath 
            -> Permissions
            -> Maybe Integer
            -> UTCTime
            -> a
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle handler $
    bracket (openFile path ReadMode) hClose $ \h -> do
        size <- hFileSize h
        return (Just size)
betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (handler) $ do
        h <- openFile path ReadMode
        size <- hFileSize h
        hClose h
        return (Just size)

myTest path _ (Just size) _ = 
    takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

pathP path _ _ _ = path

pathP :: InfoP FilePath
sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k 

equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k 

liftP:: (a->b-> c)-> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a)=> InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

liftP2 :: (a-> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP = liftP2 (&&)
orP = liftP2 (||)

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP' q f k w x y z = f w x y z `q` constP k w x y z

-- l43 in terms of equalP
liftPath :: (FilePath -> a)-> InfoP a
liftPath f w _ _ _ = f w

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
        (sizeP `greaterP` 131072)

(==?) = equalP
(&&?) = andP
(>?) = greaterP
myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

infix 4 ==?

infixr 3 &&?
infix 4 >?

myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072

