-- def getDigit(n, p):
--   if(p == 0):
--     digit = n % 1 * 10
--     return 0 if digit == 0 else Integer(digit)
--   return getDigit(n/10, p - 1)

--              start: 0

import Data.Fixed

getFirstDigit :: Integer -> Integer
getFirstDigit number = number `mod` 10


getDigitAt :: Integer -> Integer -> Integer
getDigitAt number 0 = getFirstDigit number
getDigitAt number position = getDigitAt (number `div` 10) (position - 1)

toDigits :: Integer -> [Integer]
toDigits 0      = []
toDigits number =  toDigits (number `div` 10) ++ [(getFirstDigit number)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev number = reverse (toDigits number)

main :: IO ()
main = do
  print (toDigits 123 ++ [0] ++ toDigitsRev 123)
