-- ============================================================================================================================================================
-- Autor: Mateus Fonseca
-- Data: 09/03/2020
-- ============================================================================================================================================================
-- Exercício 1 - CIS 194 https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf
--
-- Válidador de cartão de crédito.
--   • Double the value of every second digit beginning from the right.That is, the last digit is unchanged; the second-to-last
-- digit is dou-bled; the third-to-last digit is unchanged; and so on. For example, [1,3,8,6] becomes [2,3,16,6].
--   •  Add the digits of the doubled values and the undoubled dig-its from the original number. For example,[2,3,16,6]becomes2+3+1+6+6 = 18.
--   •  Calculate the remainder when the sum is divided by10. For theabove example, the remainder would be8.
--      If the result equals0, then the number is valid.

-- getFirstDigit :: Integer -> Integer
getFirstDigit number = mod number 10

-- toDigits :: Integer -> [Integer]
toDigits 0      = []
toDigits number =  toDigits (div number 10) ++ [(getFirstDigit number)]

-- toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []     = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:xs)) = [x, y * 2] ++ doubleEveryOther(xs)

-- sumDigits :: [Integer] -> Integer
sumDigits digitos = sum (map (sum . toDigits) digitos )

-- validate :: Integer -> Bool
validate cardNumber = ((mod . (sumDigits . (reverse . doubleEveryOther . toDigitsRev))) cardNumber 10) == 0


main :: IO ()
main = do
    cardNumber <- readLn :: IO Integer
    print (validate cardNumber)
