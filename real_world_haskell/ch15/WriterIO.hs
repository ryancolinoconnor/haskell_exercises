
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module WriterIO where
import System.IO (IOMode(..))
import Control.Monad (Monad)
import Control.Monad.Writer.Lazy (MonadWriter, Writer, runWriter)
data Event = Open FilePath IOMode
            | Put String String
            | Close String
            | GetContrents String
                deriving (Show)

newtype WriterIO a = W {runW :: Writer [Event] a }
        deriving (Monad, MonadWriter [Event], Functor, Applicative)

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW
