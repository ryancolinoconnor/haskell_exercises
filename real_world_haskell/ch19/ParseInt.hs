{-# LANGUAGE  GeneralizedNewtypeDeriving #-}

module ParseInt where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import MonadError

data ParseError = NumericOverflow
                | EndOfInput
                | Chatty String
                  deriving (Eq, Ord, Show)

instance MonadError.Error ParseError where
    noMsg = Chatty "oh noes!"
    strMsg = Chatty

newtype Parser a = P {
                        runP :: ExceptT ParseError (State B.ByteString) a
             } deriving (Monad, Applicative,Functor, Control.Monad.Except.MonadError ParseError)

liftP :: State B.ByteString a -> Parser a
liftP m = P (lift m)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    s <- liftP get
    case B.uncons s of
      Nothing -> Control.Monad.Except.throwError EndOfInput
      Just (c, s')
            | p c -> liftP (put s') >> return c
            | otherwise -> Control.Monad.Except.throwError (Chatty "satisfy failed")

optional :: Parser a -> Parser (Maybe a)
optional p = (Just `liftM` p) `Control.Monad.Except.catchError` \_-> return Nothing

runParser :: Parser a -> B.ByteString -> Either ParseError (a, B.ByteString)
runParser p bs = case runState (runExceptT (runP p)) bs of
                   (Left err, _) -> Left err
                   (Right r, bs) -> Right (r, bs)

-- :m +Data.Char
--
-- let p = satisfy isDigit
--
-- runParser p (B.pack "x")
--
-- runParser p (B.pack "9abc")
--
-- runParser (optional p) (B.pack "x")
--
-- runParser (optional p) (B.pack "9a")
