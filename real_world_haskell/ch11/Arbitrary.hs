{-# LANGUAGE ConstrainedClassMethods, TypeSynonymInstances, FlexibleInstances#-}
module Arbitrary where
import Test.QuickCheck (Gen, choose)
import System.Random 
class Arbitrary a where
    arbitrary :: Gen a
    elements :: [a] -> Gen arbitrary
    choose :: Random a => (a, a) -> Gen a
    oneof :: [Gen a] -> Gen a

data Ternary
    = Yes
    | No
    | Unknown 
    deriving (Eq, Show)

--instance Arbitrary Ternary where
 --   arbitrary = elements [Yes, No, Unknown]

instance Arbitrary Ternary where 
    arbitrary = do
        n <- Test.QuickCheck.choose (0, 2) :: Gen Int
        return $ case n of 
                   0 -> Yes
                   1 -> No
                   _ -> Unknown

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (x, y)

instance Arbitrary Char where
    arbitrary = elements (['A'..'Z'] ++ ['a' ..'z'] ++ " !!@#$%^&*()")


instance Arbitrary String where
    arbitrary = do
        n <- Test.QuickCheck.choose (0, 2) :: Gen Int
        return $ case n of 
                   0 -> "Yes"
                   1 -> "No"
                   _ -> "Unknown"
