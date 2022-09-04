-- todo: come back to this exercise set and iron out some of the issues, 
import Data.Char(toUpper,toLower)
import System.Directory (doesDirectoryExist, doesFileExist,
                        getCurrentDirectory, getDirectoryContents)
import           Control.Monad    (filterM,liftM)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath  ((</>))

import Text.Regex.Posix ((=~))
import System.FilePath (dropTrailingPathSeparator, splitFileName,(</>))
import Control.Exception (handle,SomeException)
import Control.Monad (forM)
-- pass "[" to globToRegex
-- 
--
-- globToRegex "mysample[]"
--"^mysample[]*** Exception: unterminated character class
--
--
-- 2. Add a param to globToRegrex and matchesGlob to control case sensitive
-- if you parameterize the escape c to be (c|(toUpper c))
-- this also handles the exercises at the end of the chapter
-- 1. Write a version of globToRegex that uses the type signature shown earlier
-- j
-- This is quite annoying
type GlobError =  String
eitherHandler prep res = case res of
                        Left err -> Left err
                        Right str -> Right (prep ++ str)
eitherHandlerPost res ( prep)  = case res of 
                        Left err -> Left err
                        Right str -> Right ( str ++ prep)
globToRegex :: String->Bool -> Prelude.Either GlobError String
globToRegex cs caseSensitive= (['^'] `eitherHandler`  (globToRegex' cs caseSensitive)) `eitherHandlerPost`  "$"

globToRegex' ::String -> Bool -> Prelude.Either GlobError String
globToRegex' "" caseSensitive = Right ""
globToRegex' ('*':cs) caseSensitive  = ".*" `eitherHandler` (globToRegex' cs caseSensitive)
globToRegex' ('?':cs) caseSensitive= "." `eitherHandler` (globToRegex' cs caseSensitive)
globToRegex' ('[':'!':c:cs) caseSensitive = ("[^" ++ [c]) `eitherHandler` (charClass cs caseSensitive)
globToRegex' ('[':c:cs) caseSensitive = (("[")++[c]) `eitherHandler` charClass cs caseSensitive
globToRegex' ('[':_) caseSensitive = Left "unterminated character class"
globToRegex' (c:cs) caseSensitive = (escape c caseSensitive) `eitherHandler`  globToRegex' cs caseSensitive

escape :: Char->Bool -> String
escape c caseSensitive| c `elem` regexChars = '\\' : [c]
                      | otherwise = if caseSensitive then [c]
                                                     else "(" ++ [(toLower c)] ++ "|" ++ [(toUpper c)] ++ ")"
        where regexChars = "\\+()^$.{}]|"
              
charClass::String->Bool->Prelude.Either GlobError String
charClass (']':cs) caseSensitive = [']'] `eitherHandler` (globToRegex' cs caseSensitive)
charClass (c:cs) caseSensitive = [c] `eitherHandler` charClass cs caseSensitive
charClass [] _ = Left "unterminated character class"

matchesGlob:: FilePath -> String ->Bool-> Prelude.Either GlobError Bool
matchesGlob name pat caseSensitive = 
    let res =globToRegex' pat caseSensitive in 
    case res of
    Left err -> Left err
    Right regex ->Right (name =~ regex)

matchesGlobEither:: String-> String->Bool
matchesGlobEither name pat = let res = matchesGlob name pat False
                            in case res of
                            Left err -> False
                            Right b -> b
-- "Foo.c" =~ globToRegex "f??.c" False::Bool
-- True
-- "Foo.c" =~ globToRegex "f??.c" True::Bool
-- False
--
--
-- before "Handling Errors through API design"
-- 1. Windows: pathSeparator == '\\'
-- Posix:   pathSeparator ==  '/'
-- it's simply applying the appropriate case sensitivity based on the pathsep, though there really should be a better checker
-- 2. fileExist under: https://hackage.haskell.org/package/unix-2.8.0.0/docs/System-Posix-Files.html
-- 3. Implement "**.c"
-- modify namesmatching to handle Errors 
concatM arr = Prelude.fmap concat arr
getAllDirsM filePath = liftM getAllDirs filePath
getAllDirs filePath = do
            paths<- getDirectoryContents filePath
            let plus_paths = map (filePath </>) paths
            let filtered_paths = filterM clear_filepath plus_paths
            filtered_paths
-- gets directories and subdirectories of a given dir
getDirectories  filePath =do 
                let dirs= getAllDirs filePath  
                output_dirs <- dirs
                if (null output_dirs)  then 
                    return output_dirs
                else
                    do 
                    all_dirs <- mapM (getDirectories) output_dirs
                    return (output_dirs ++ (concat all_dirs))

clear_filepath:: FilePath ->  IO Bool
clear_filepath pn |last pn =='.'=return  False
clear_filepath pn= doesDirectoryExist pn
malformed = globToRegex "some malformed string["
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")
getSubDirs pn  = getDirectories pn

listMatches :: FilePath -> String -> IO (Prelude.Either GlobError [FilePath])
listMatches dirName pat = do
        dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
        names <- getDirectoryContents dirName'-- current dir or specified
        let names'  = if isHidden pat
                    then filter isHidden names
                    else filter (not . isHidden) names
        let check = matchesGlob (head names')  pat False
        case check of
              Left err-> return $ (Left err)
              Right bool ->  (return $ Right (filter (`matchesGlobEither` pat) names'))
        where handler:: SomeException -> IO [String]
              handler _ = ( (return [])) 

isHidden ('.':_) = True
isHidden _ = False
doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
       then return True
       else doesDirectoryExist name
--listPlain :: FilePath -> String -> Either GlobError [String]
--listPlain dirName baseName = do
--    exists <- if null baseName
--              then Right (doesDirectoryExist dirName)
----              else Right (doesNameExist (dirName </> baseName))
--    case exists of
 --     Left err -> return err
  --    Right exists' -> if exists' then
   --                        return [baseName]
    --                    else
     --                       return []
-- didn't quite test this as it's massively painful
deright_filepath:: Either GlobError FilePath-> FilePath
deright_filepath (Right path) = path
deright_filepathlist:: Either GlobError [FilePath]-> [FilePath]
deright_filepathlist (Right path) = path
deleft_err:: Either GlobError FilePath -> GlobError
deleft_err (Left err) = err
iserr a = case a of
            Right _ ->  False
            Left _ ->  True
noterr a = case a of 
            Right _ ->  True
            Left _ ->  False
either_map2 ::  ([FilePath])-> (
    FilePath->String->IO (Either GlobError [FilePath]))->FilePath -> IO (Either GlobError [FilePath])
either_map2 dirs listDir baseName  = do 
                                pathnames <- forM dirs $ \dir -> do
                                        baseNames <- listDir dir baseName
                                        case baseNames of
                                          Left err -> return (Left err)
                                          Right baseNames' -> return (Right (map (dir </>) baseNames'))
                                let errs = filter iserr pathnames 
                                let noterrs = filter noterr pathnames 
                                if null errs then
                                    let noterrs' = concat (map deright_filepathlist noterrs) in 

                                    return (Right ( noterrs'))
                                else
                                    return ( ( (head errs)))
either_baseNames ::  (Either GlobError [FilePath])-> (
    FilePath->String->IO (Either GlobError [FilePath]))->FilePath -> IO (Either GlobError [FilePath])
either_baseNames dirs listDir baseName = case dirs of 
                                  Left err -> return (Left err)
                                  Right dirs' -> either_map2 dirs' listDir baseName 
                                  --Right dirs' -> (Right (forM dirs' $ \dir -> do 
                                   --     baseNames <- listDir dir baseName
                                   --     return (map (dir </>) baseNames)))

namesMatching :: String -> IO (Prelude.Either GlobError [FilePath])
namesMatching pat  
                    | not (isPattern pat) = do
                        exists <- doesNameExist pat
                        return (if exists  then Right [pat] else Right [])
                    | otherwise = do
                        case splitFileName pat of
                          ("",baseName) -> do
                              curDir <- getCurrentDirectory
                              listMatches curDir baseName
                          (dirName, baseName) -> do
                              dirs <- if isPattern dirName
                                then namesMatching (dropTrailingPathSeparator dirName)
                                else if (take 2 baseName)=="**"
                                    then
                                        do
                                        subdirs <- getSubDirs dirName
                                        return  (Right (subdirs))
                                    else return (Right [dirName])
                              let listDir = if take 2 baseName == "**" 
                                            then listMatches  . tail 
                                            else if isPattern baseName
                                                then listMatches
                                            else listMatches
                              pathNames <- (either_baseNames dirs listDir baseName)
                              case pathNames of
                                Left err -> return $ Left err
                                Right pns -> return $Right (concat pathNames)
-- 1. Glob pattens are simple enough to interpet that it's easy to write a matcher in haskell rather than using the regex machinery give it a try
-- rather tedious so i'm not matching everything, and haven't used much more than *, ? personally
-- pattern, file, output
takelast 0 _ = []
takelast n list = (takelast (n-1) (init list)) ++ [tail list]
matchesHask :: String -> String-> Bool
matchesHask "*" _ = True
matchesHask "" _ = False
matchesHask ('?':pat) (_:str_rest) = matchesHask pat str_rest
matchesHask ('*':end) str = let len_ = (min (length end) (length str)) in
                        (takelast len_ end)==(takelast len_ str)
matchesHask pat filename = pat==filename
                    
