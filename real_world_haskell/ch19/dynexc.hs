{- # LANGUAGE DerivaeDataTypeable #-}

import Data.Dynamic 
import Control.Exception 

data SqlError = SqlError {setState :: String,
                          setNativeError :: Int,
                          setErrorMsg :: String
                         }
                deriving (Eq, Show, Read, Typeable)

{- | Execute the given IO action.

If it raises a 'SqlError', then execute the supplied handler and return its return value.
    Otherwise proceeed as normal. -}
catchSql :: IO a -> (SqlError -> IO a) -> IO a
catchSql = catchDyn

{- | Like `catchSql`, with the order of arguments reverse. -}
handleSql :: (SqlError -> IO a) -> IO a -> IO a
handleSql = flip catchSql

handleSqlError :: IO a-> IO a
handleSqlError action = 
    catchSql action handler 
        where handler e = fail ("SQL Error: " ++ show e)

throwSqlError :: String -> Int -> String -> a
throwSqlError state nativeerror errormsg =
    throwDyn (SqlError state nativeerror errormsg)

throwSqlErrorIO :: String -> Int -> String -> IO a
throwSqlErrorIO state nativeerror errormsg =
    evaluate (throwSqlError state nativeerror errormsg)
