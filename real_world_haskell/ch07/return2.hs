-- more "puter version"
import Data.Char(toUpper)
isYes :: String -> Bool
isYes inpStr = (toUpper . head $ inpStr) == 'Y'
isGreen :: IO Bool
isGreen =
    do putStrLn "Is green your favorit color?"
       inpStr <- getLine
       return (isYes inpStr)
