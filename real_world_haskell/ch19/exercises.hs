import ParseInt
import Data.Char

import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import MonadError
-- 1.1 Take the Either Example and make it work with laziness in the style of the maybe example
divBy :: Integral a => a -> [a] -> [Either String a]
divBy numerator denominators = 
    map worker denominators
    where worker 0 = Left "Division by zero error"
          worker x = Right (numerator `div` x)

-- divBy 50 [1,2,5,8,10]
-- divBy 50 [1,2,0,8,10]
-- take 5 (divBy 100 [1..])


-- This can obviously be extended to raise the errors e.g. in version 7
data DivByError a = DivBy0
                    | ForbiddenDenominator a
                    deriving (Eq, Read, Show)

divByCustom :: Integral a => a -> [a] -> [Either (DivByError a) a]
divByCustom numerator denominators = 
    map worker denominators
    where worker 0 = Left DivBy0
          worker x = Right (numerator `div` x)

-- divByCustom 50 [1,2,5,8,10]
-- divByCustom 50 [1,2,0,8,10]
-- take 5 (divBy 100 [1..])

-- 2.1 Write a many parser with type Parser a -> Parser [a]
-- it should apply a parser until it failes
-- comment: this is strange you'd excpet a list of left htne right


abstractMany p bs = case runState  (runExceptT (runP p)) bs of
                       (Left err,_) ->  ([],bs)
                       (Right r, bs') -> let (outs, bsNew) =  (abstractMany p bs')
                                          in  ([r] ++ outs, bsNew)


many :: Parser a -> Parser [a]
many p = do 
    s <- liftP get
    let (ret, bsNew) = (abstractMany p s) 
        in liftP (put bsNew) >> return ret


-- :m +Data.Char
--
--
-- let p = satisfy isDigit
--
-- let p' = many2 p
--
-- runParser p' (B.pack "999ac")

-- 2.2 use many to write an int parser

isDigitOrMinus :: Char-> Bool 
isDigitOrMinus c = c == '-' || isDigit c

readInt :: [Char] -> Int
readInt str = read str ::Int

intParser :: Parser Int
intParser = readInt `liftM` (many (satisfy isDigit)) 
            
-- let p = intParser
--
-- runParser p (B.pack "999ac")
--
-- 2.3 Modify your parse to throw a NumericOverflow if it detects a numeric overflow
-- hacky solution below, what should really be done is to add in like a foldr, or a way to pass in an aggregator to many- the contstraint of a->[a] is dumb, ideally what you'd like to do is as you read in the characters accumulate, and throw the error as necessary, there may be some way of doign that per their signature specs but I don't know it 
runParser2 p bs = case runState (runExceptT (runP p)) bs of
                   (Left err, _) -> Left err
                   (Right r, bs) -> Right (r, bs)

intParserException :: Parser Int
intParserException = do
    let p = many (satisfy isDigit) 
    s <- liftP get
    case runParser2 p s of
        Left err -> Control.Monad.Except.throwError (Chatty "Uncaught err")
        Right (r,bs) -> case readIntException r of 
            Nothing -> Control.Monad.Except.throwError NumericOverflow
            Just out -> liftP (put bs) >> return out

readIntException :: [Char] -> Maybe Int
readIntException str = let out = read str ::Int
                in if (length (show (out)::[Char])) < length str then
                    Nothing
                else
                    Just out

-- let p = intParserException
--
-- let good_str_ = show (maxBound::Int) 
--
-- runParser p (B.pack good_str_)
--
-- let str_ = show (maxBound::Int) ++ "10j"
--
-- runParser p (B.pack str_)
