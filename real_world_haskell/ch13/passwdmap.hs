import Data.List
import qualified Data.Map as Map
import System.IO
import Text.Printf(printf)
import System.Environment(getArgs)
import System.Exit
import Control.Monad(when
{- Fields of /etc/passwd file in POSIX systems -}
data PasswdEntry = PasswdEntry {
    userName :: String,
    password :: String,
    uid :: Integer,
    gid :: Integer,
    gecos :: String,
    shell :: String}
    deriving (Eq, Ord)

{- | Define how we get data to a 'PasswdEntry'. -}
instance Show PasswdEntry where
    show pe = printf "%s:%s:%d:%d:%s:%s:%s"
    (userName pe) (password pe) (uid pe) (gid pe)
    (gecos pe) (homeDir pe) (shell pe)
-- converting data from entry
instance Read PasswdEntry where
    readsPrec _ value = 
        case split ':' value of
            [f1, f2, f3, f4, f5, f6,f7]-> 
                [(PasswdEntry f1, f2, (read f3), (read f4), f5, f6,f7,[])]
            x -> error $ "Invalid number of fields in input:" ++ show x
        where
        split :: Eq a => a -> [a] -> [[a]]
        split _ [] = [[]]
        split delim str =
            let 
                (before,remained) = span (/= delim) str
                in
                before : case remainder of
                        [] -> []
                        x -> split delim (tail x)


-- for convenience
type UIDMap = Map.Map Integer PasswdEntry
type UserMap = Map.Map String PasswdEntry

inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps inp =
    (uidmap, usermap)
        where
            uidmap = Map.fromList . map (\pe -> (iod pe, pe)) $ entries
            usermap = Map.fromList . map (\pe -> (userName pe, pe)) $ entries
        -- Convvert the input Strign to [PasswdEntry]
        entries = map read (lines inp)

main = do
    args <- getArgs
    when (length args /= 1) $ do
        putStrLn "Syntax: passwdmap filename"
        exitFailure

    content <- readFile (head args)
    let maps = inputToMaps content
    mainMenu maps

mainMenu maps@(uidmap, usermap) = do
    putStr optionText
    hFlush stdout
    sel <- getLine
    case sel of 
        "1" -> lookupUserName >> mainMenu maps
        "2" -> lookupUID >> mainMenu maps
        "3" -> displayFile >> mainMenu maps
        "4" -> return ()
        _ -> putStrLn "Invalid selection" >> mainMenu maps
        where
        lookupUserName = do
            putStrLn "Username: "
            username <- getLine
            case Map.lookup username usermap of
                Nothing -> putStrLn "Not found."
                Just x -> print x
        lookupUID = do
            putStrLn "UID: "
            uidString <- getLine
            case Map.lookup (read uidstring) uidmap of
              Nothing -> putStrLn "Not found."
              Just x -> print x
        displayFile = putStr . unlines . map (show .snd) . Map.toList $ uidmap
        optionText = 
            "\n passwdmap options:\n\
            \\n\
            \1 Look up username\n\
            \2 Look up a UID\n\
            \3 display file\n\
            \4 Quit\n\n\
            \your selection: "

              




