module ControlledVisit  where 
import Control.Monad (filterM, liftM, forM)
import System.Directory (Permissions(..),getDirectoryContents, getModificationTime, getPermissions)
--import System.Time (ClockTime(..))
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension, (</>))
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Control.Exception (SomeException)
import RecursiveContents (getRecursiveContents)
data Info = Info {
    infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)


traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
        if isDirectory info && infoPath info /= path
           then ControlledVisit.traverse order (infoPath info)
           else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (handler) (Just `liftM` act)
            where handler :: SomeException -> IO (Maybe a)
                  handler _ = return Nothing

getInfo :: FilePath -> IO Info
getInfo path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    return (Info path perms size modified)

traverseVerbose order path = do
        names <- getDirectoryContents path
        let usefulNames = filter (`notElem` [".",".."]) names
        contents <- mapM getEntryName ("":usefulNames)
        recursivecontents <- mapM recurse (order contents)
        return (concat recursivecontents)
        where getEntryName name = getInfo (path </> name)
              isDirectory info = case infoPerms info of
                                Nothing -> False
                                Just perms -> searchable perms
              recurse info = do
                    if isDirectory info && infoPath info /= path
                       then traverseVerbose order (infoPath info)
                       else return [info]
