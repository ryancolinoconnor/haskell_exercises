import Data.Char(toUpper)

isGreen :: IO Bool
isGreen =
    do putStrLn "Is green your favorit color?"
       inpStr <- getLine
       return ((toUpper . head $ inpStr)=='Y')
