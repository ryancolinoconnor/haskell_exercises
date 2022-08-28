import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception(catch,Exception)
import Control.Exception(finally)

main :: IO ()
main = withTempFile "mytemp.txt" myAction
-- guts of the program... myaction called with tempfile

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = 
    do -- Start by displaying a Greetings
        putStrLn "Welcome to tempfile.hs"
        putStrLn $ "I have a temporary file at " ++ tempname
        -- what's initial pos
        pos <- hTell temph
        putStrLn $ "My initial position is " ++ show pos

        -- Now, write some data to the temporary file
        let tempdata = show [1..10]
        putStrLn $ "Writing one line containing " ++
            show (length tempdata) ++ " bytes: " ++
                tempdata
        hPutStrLn temph tempdata

        --get new position. doesnt modify psition in memory
        --but makes pos correspond to a diff
        pos <- hTell temph
        putStrLn $ "After writing, my new position is " ++ show pos

        -- >>Seek beginning
        putStrLn $ "The file content is: "
        hSeek temph AbsoluteSeek 0

        -- hget contents is a lazy Read
        --
        c <- hGetContents temph
        putStrLn c
        putStrLn $ "Which could be expressed as this Haskell literal:"
        print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do -- lirary ref said that it may raise exceptions
        --- so we run it under catch
        -- if catchtempdirectory makes
        -- and issue then we return the current
        -- call 
        tempdir <- (getTemporaryDirectory) --(\_ -> return ".") 
        (tempfile, temph) <- openTempFile tempdir pattern
        finally (func tempfile temph)
            (do hClose temph
                removeFile tempfile)
-- Welcome to tempfile.hs
-- I have a temporary file at /tmp/mytemp2460-0.txt
-- My initial position is 0
-- Writing one line containing 22 bytes: [1,2,3,4,5,6,7,8,9,10]
-- After writing, my new position is 23
-- The file content is:
--- [1,2,3,4,5,6,7,8,9,10]

-- Which could be expressed as this Haskell literal:
---"[1,2,3,4,5,6,7,8,9,10]\n"
