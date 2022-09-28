import GHC.Exception (ArithException(DivideByZero))
import Control.Exception

catchIt :: ArithException -> Maybe ()
catchIt (DivideByZero) = Just ()
catchIt _ = Nothing

handler :: () -> IO ()
handler _ = putStrLn "Caught divby0"

safePrint :: Integer -> IO ()
safePrint x = handleJust catchIt handler (print x)

-- let x = 5 `div` 0
--
-- let y = 5 `div` 1
--
-- safePrint x
--
-- safePrint y
