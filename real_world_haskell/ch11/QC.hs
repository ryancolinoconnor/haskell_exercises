import Arbitrary
import Prettify2
import Test.QuickCheck (Gen, choose)
import Control.Monad (liftM,liftM2)
import Data.Monoid ( mappend, mempty)
--instance Arbitrary Doc where
    --arbitrary = do
     --   n <- Test.QuickCheck.choose (1,6) :: Gen Int
      --  case n of 
       --   1 -> return Empty
        --  2 -> do x <- arbitrary
         --         return (Char x)
          --3 -> do x <- arbitrary
           --       return (Text x)
          --4 -> return Line
          --5 -> do x <- arbitrary
            --      y <- arbitrary
           --       return (Concat x y)
          --6 -> do x <- arbitrary
           --       y <- arbitrary
            --      return (Union x y)


instance Arbitrary Doc where
    arbitrary = 
        oneof [ return Empty
              , liftM Char arbitrary
              , liftM Text arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary]

prop_empty_id x = empty Prettify2.<> x == x
    && 
        x Prettify2.<> empty == x

prop_char c = char c == Char c
prop_text s = text s == if null s then Empty else Text s
prop_line = line ==Line
prop_double d = double d == text (show d)

prop_hcat xs = hcat xs == glue xs
    where 
        glue [] = empty
        glue (d:ds) = d Prettify2.<> glue ds
-- doesn't work because we haven't written instance of intersperse
--prop_punctuate s xs = punctuate s xs == intersperse s xs
--prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
--    where 
--        combine [] = []
--        combine [x] = [x]
--        combine (x:Empty:ys) = x : combine ys
--        combine (Empty:y:ys) = y: combine ys
--        combine (x:y:ys) = x `Concat` y: combine ys

prop_mempty_id x = 
    mempty `mappend` x ==x
    &&
        x `mappend` mempty == (x)
