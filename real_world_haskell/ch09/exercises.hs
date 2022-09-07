import System.Posix.Files (FileStatus, getFileStatus,fileOwner)
import Control.Monad (filterM, liftM, forM)
import System.Directory (Permissions(..),getDirectoryContents, getModificationTime, getPermissions)
--import System.Time (ClockTime(..))
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension, (</>))
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Control.Exception (SomeException)
import RecursiveContents (getRecursiveContents)
import System.Directory (Permissions(..),getDirectoryContents, getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime)
import Control.Monad (filterM)
import ControlledVisit 
import FoldDir
import Data.Char (toLower)
import System.FilePath (takeExtension, (</>))
--1.1 is the order in which we call bracket and handle important? why?
-- context:
-- getFileSize path = handle (\_ -> return Nothing) %
-- │   bracket (openFile path ReadMode) hClose $ \h -> do
-- │   │   size <- hFileSize h
-- │   │   return (Just size)
-- it seems in this case putting the handle outside will help to deal with issues of opening the fil
-- 2.1 What should you pass to traverse to traverse in reverse alphabetic order
-- sort reverse infoPath
--2.2 Using id as a control function traverse id performs a preorder traversal of a tree: it returns parent before it's children, how to put child before 
-- technically this happens because in the third line we put path first, so simply reversing the list will to it. i.e. 
-- Main.traverse reverse "./", or since the alphabetical repr of the parent will always be less than the first, you can use the answer from 2.1
-- 2.3 Take the predicates from Gluing predicates together and make them work with new info 

type InfoP' a = Info -> a
simpleAndInfo :: InfoP' Bool -> InfoP' Bool -> InfoP' Bool
simpleAndInfo f g x  = f x && g x 

liftInfo2 :: (a-> b -> c) -> InfoP' a -> InfoP' b -> InfoP' c
liftInfo2 q f g x = f x `q` g x

andInfo = liftInfo2 (&&)
orInfo = liftInfo2 (||)

constInfo :: a-> InfoP' a
constInfo k _  = k

liftInfo' q f k w x y z = f w x y z `q` constInfo k w x y z
-- 2.4 Write a wrapper for traverse that let's you control traversal using one predicate and filter results using another
-- This really isn't the greatest- ideally you could filter as you go...
-- Not sure if mapm allows to to apply a filter as well
traverseFilter ::(Info->Bool)-> ([Info]->[Info]) -> FilePath -> IO [Info]
traverseFilter f order path = do 
                                names <- ControlledVisit.traverse order path
                                return (filter f names)

-- 3.1 Modify foldTree to allow the caller to change the order of traversal of entries
foldTreeOrdered :: (IO [String] -> IO [String]) -> Iterator a -> a -> FilePath -> IO a
foldTreeOrdered order iter initSeed path = do
        endSeed <- fold order initSeed path
        return (unwrap endSeed)
    where
        fold order seed subpath = order (getUsefulContents subpath) >>= walk order seed
        walk order seed (name:names) = do
            let path' = path </> name
            info <- getInfo path'
            case iter seed info of
                done@(Done _) -> return done
                Skip seed' -> walk order seed' names
                Continue seed'
                    | isDirectory info -> do
                        next <- fold order seed' path'
                        case next of
                            done@(Done _) -> return done
                            seed'' -> walk order (unwrap seed'') names
                    | otherwise -> walk order seed' names
        walk order seed _ = return (Continue seed)
-- 3.2 Modify foldTree to allow the user to specify pre order traversal so that it could return children
-- before parents -- this is quite bad actually because if we were to create a directory level only going a couple of levels
-- it would be harder to prevent-- also might be a tad bit wrong in some cases but seems correct for below test case
-- foldTreePreOrder True/False accDirectories [] "../"
foldTreePreOrder ::Bool-> Iterator a -> a -> FilePath -> IO a
foldTreePreOrder preorder iter initSeed path = do
        endSeed <- fold preorder initSeed path
        return (unwrap endSeed)
    where
        fold preorder seed subpath = getUsefulContents subpath >>= walk preorder seed
        walk preorder seed (name:names) = do
            let path' = path </> name
            info <- getInfo path'
            if isDirectory info then
                if not preorder then
                    do 
                        next <- fold preorder seed path'
                        case next of 
                            done@(Done _) -> return done
                            seed'' -> case iter (unwrap seed'') info of
                                        done@(Done _) -> return done
                                        Skip seed' -> walk preorder seed' names
                                        Continue seed' ->
                                            walk preorder (seed') names
                else
                    case iter seed info of
                        done@(Done _) -> return done
                        Skip seed' -> walk preorder seed' names
                        Continue seed' -> do
                            next <- fold preorder seed' path'
                            case next of
                                done@(Done _) -> return done
                                seed'' -> walk preorder (unwrap seed'') names
            else
                case iter seed info of
                    done@(Done _) -> return done
                    Skip seed' -> walk preorder seed' names
                    Continue seed'
                        | isDirectory info -> do
                            next <- fold preorder seed' path'
                            case next of
                                done@(Done _) -> return done
                                seed'' -> walk preorder (unwrap seed'') names
                        | otherwise -> walk preorder seed' names
        walk preorder seed _ = return (Continue seed)
accDirectories lis info = Continue (lis ++ [ControlledVisit.infoPath info])

-- 3.3 Combinator library that makes it possible to express the kinds of iterators that foldTree Accepts
-- It's unclear to me what they're asking for here are we trying to combine iterators? 
-- I also think the iterators are relatively succinct
-- There seems to also be debate online about what a combinator library
-- Applying iterators on top of each other is OK I guess, but how to handle skip/Continue
-- Done + skip/continue should be done, but maybe they mean in terms of the "betterFind"
-- Which just seems like copy pasting predicates from before stuff like liftp and so forth, skipping for now
-- 4.1/4.2 Add information about who owns a directory
-- Writing this via WSL2/ Ubuntu so going with posix
-- The Posix API interacts super cleanly with the base filedirectory as below, being that I'm
-- time boxed i'm not porting all of the code, but below is an example using the system.posix api to
-- accumulate the owners. To make it available to predicates you'd want to 
-- Add the fileOwner as an attribute directly, but having the fileStatus gives a lot more flexibility
-- as the api is FilePath-> FileStatus -> all of the other good things
data InfoPosix = InfoPosix {
    fInfo :: Info
  , fileStatus :: FileStatus
  } 

getInfoPosix :: FilePath -> IO InfoPosix
getInfoPosix path = do
    perms <- maybeIO (getPermissions path)
    size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
    modified <- maybeIO (getModificationTime path)
    fileStatus <-  (getFileStatus path) 
    return (InfoPosix (Info path perms size modified) fileStatus)
accOwners lis info = Continue (lis ++ [((infoPath (fInfo info)), (fileOwner (fileStatus info)))])
type IteratorPosix  seed = seed -> InfoPosix -> Iterate seed
foldTreePosix :: IteratorPosix a -> a -> FilePath -> IO a
foldTreePosix iter initSeed path = do
        endSeed <- fold initSeed path
        return (unwrap endSeed)
    where
        fold seed subpath = getUsefulContents subpath >>= walk seed
        walk seed (name:names) = do
            let path' = path </> name
            info <- getInfoPosix path'
            case iter seed info of
                done@(Done _) -> return done
                Skip seed' -> walk seed' names
                Continue seed'
                    | isDirectory (fInfo info) -> do
                        next <- fold seed' path'
                        case next of
                            done@(Done _) -> return done
                            seed'' -> walk (unwrap seed'') names
                    | otherwise -> walk seed' names
        walk seed _ = return (Continue seed)
