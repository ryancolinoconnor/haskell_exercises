{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

import Control.Monad.Except (ExceptT, runExceptT)

import Control.Monad.State
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as L
import UglyStack
import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.Writer (WriterT, tell)
import Control.Monad.State
-- 1.1 Modify the App type synonym to swap the order of ReaderT and WriterT what effect does this have on runApp execution function
--type AppRev =  AppConfig (StateT AppState IO) ReaderT
-- it seems you'd have to swap the order in which you call runreaderT and runstate or else it wouldn't wrok
--
-- 1.2 Add the WriterT transformer to the App monad transformer stack. Modify runApp to work with this new setup
type AppWT =  (ReaderT  AppConfig (StateT AppState (WriterT [(FilePath, Int)] IO)) ) 


runAppWT k maxDepth = 
    let config = AppConfig maxDepth
        state = AppState 0
        func =  runReaderT k
    in runStateT (func config) state




-- 1.3 Modify ConstrainedCount to record the results using the WriterT transformet in the new app stack
constrainedCount1 :: Int -> FilePath -> AppWT [(FilePath, Int)]
constrainedCount1 curDepth path = do
    contents <- liftIO . listDirectory $ path
    tell [(path, length contents)]
    cfg <- ask
    rest <- forM contents $ \name -> do
        let newPath = path </> name
        isDir <- liftIO $ doesDirectoryExist newPath
        if isDir && (curDepth < cfgMaxDepth cfg)
           then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $ 
                   put st { stDeepestReached= newDepth }
                constrainedCount1 newDepth newPath
            else return []
    return $ (path, length contents) : concat rest


-- --
-- -- usage
-- let app = constrainedCount1 0 ".."
-- let out = runAppWT app 3

-- 2.1 Our Parse Monad isn't a perfect replacement because maybe doesn't give useful info if parse fails, create an eithertype sometype monad transformer and use it to create a more capable parse monad
-- The problem is the given monad for maybet doesn't even compile :'(
-- looks like exceptT is the standard that should be used according to stack overflow:TmuxNavigateRight


data ParseState = ParseState {
    string :: L.ByteString
  , offset :: Int64
                  } deriving (Show)

-- newtype Parse m = ExceptT ((State ParseState) m)
  
data Err = Err deriving (Show)

newtype Parse  a = P {
    runP :: ExceptT Err  (State ParseState) a
                    } deriving (Functor, Applicative, Monad, MonadState ParseState)

evalParse :: Parse a -> L.ByteString -> Either Err  a
evalParse m s = evalState (runExceptT (runP m)) (ParseState s 0)
