import GHC.Exception (ArithException(DivideByZero))
import Control.Exception

-- No longer working due to changes in the errros framework
catchIt :: ArithException -> Maybe ()
catchIt (DivideByZero) = Just ()
catchIt _ = Nothing

handler :: () -> IO ()
handler e = putStrLn $"Caught arithmetic error: " ++ show e

safePrint :: Integer -> IO ()
safePrint x = handleJust catchIt  handler (print x)

-- let x = 5 `div` 0
--
-- let y = 5 `div` 1
--
-- --doesn't actually show it
-- safePrint x
--
-- safePrint y
