{-# LANGUAGE FlexibleContexts, DatatypeContexts #-}
-- This does not work due to removal of the old error system
import Control.Monad.Except

data Show a =>
    DivByError a = DivBy0
                   | ForbiddenDenominator a
                   | OtherDivByError String
                     deriving (Eq, Read, Show)

instance Except (DivByError a) where
    msg x = OtherDivByError x

divBy :: Integral a => a -> [a] -> Either (DivByError a) [a]
divBy = divByGeneric

divByGeneric :: (Integral a, MonadError (DivByError a) m) =>
    a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0:_) = throwError DivBy0
divByGeneric _ (10:_) = throwError (ForbiddenDenominator 10)
divByGeneric numerator (denom:xs) = 
                   case divByGeneric numerator xs of 
                     Left x -> Left x
                     Right results -> return ((numerator `div` denom) :results)

-- divBy 50 [1,2,5,8,10]
--
-- divBy 50 [1,2,0,8,10]
--
--
