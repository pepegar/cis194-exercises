{-
Name: Jose Luis Garcia
Collaborators: none
Notes: * I struggled with doubleEveryOther function, it was hard to me to find an
         elegant solution, and I thing that the current one is not completely
         idiomatic
       * I found the bind operator REALLY cool
-}

module HW01 where         -- We'll learn more about this later

import qualified Control.Monad as M

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.

lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

dropLastDigit :: Integer -> Integer
dropLastDigit x
    | x < 10    = x
    | otherwise = read $ init $ show x

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0    = []
    | x < 10    = [x]
    | otherwise = toDigits (dropLastDigit x) ++ [lastDigit x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse $ zipWith (*) (cycle [1,2]) $ reverse x

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ xs >>= toDigits

validate :: Integer -> Bool
validate x = 0 == sumDigits (doubleEveryOther $ toDigits x) `mod` 10
