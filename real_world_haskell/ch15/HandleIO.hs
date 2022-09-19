
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HandleIO
    (
    HandleIO
  , IOMode(..)
  , runHandleIO
  , openFile
  , hClose
  , hPutStrLn
    ) where

import Control.Monad.Trans (MonadIO(..))
import System.IO (Handle, IOMode(..))
import System.Directory (removeFile)
import qualified System.IO

newtype HandleIO a = HandleIO {runHandleIO :: IO a}
    deriving (Monad, Applicative,Functor)

openFile :: FilePath -> IOMode -> HandleIO Handle
openFile path mode = HandleIO (System.IO.openFile path mode)

hClose :: Handle -> HandleIO ()
hClose = HandleIO . System.IO.hClose

hPutStrLn :: Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO (System.IO.hPutStrLn h s)

safeHello :: FilePath -> HandleIO () 
safeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "hello world"
    hClose h

instance MonadIO HandleIO where
    liftIO = HandleIO

tidyHello :: FilePath -> HandleIO ()
tidyHello path = do
    safeHello path
    liftIO (removeFile path)



