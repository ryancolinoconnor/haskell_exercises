{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module MonadHandleIO where
import MonadHandle
import qualified System.IO
import System.IO (IOMode(..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import System.Directory (removeFile)
instance MonadHandle System.IO.Handle IO where
    openFile = System.IO.openFile
    hPutStr = System.IO.hPutStr
    hClose = System.IO.hClose
    hPutStrLn = System.IO.hPutStrLn
    hGetContents = System.IO.hGetContents

-- usage safeHello: hello to fans
safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "hello world"
    hClose h
safeRead  :: MonadHandle h m => FilePath -> m String
safeRead path = do
    h <- openFile path ReadMode
    hGetContents h

class (MonadHandle h m, MonadIO m) => MonadHandleIO h m | m -> h

instance MonadHandleIO System.IO.Handle IO

tidierHello :: (MonadHandleIO h m) => FilePath -> m ()
tidierHello path = do
    safeHello path
    liftIO (removeFile path)
    
