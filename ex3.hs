-- ================================
-- EX 1
-- ================================
length' :: [a] -> Integer
length' [] = 0
length' xs = 1 + length' (tail xs)

-- ================================
-- EX 2
-- ================================
mean :: [Integer] -> Double
mean []     = 0.0
mean xs     = sum' / size
              where sum' = fromIntegral (sum xs)
                    size = fromIntegral (length' xs)

-- ================================
-- EX 3
-- ================================
toPal :: [a] -> [a]
toPal [] = []
toPal xs = xs ++ reverse xs

-- ================================
-- EX 4
-- ================================
isPal :: [a] -> Bool
isPal [] = True
isPal xs = xs == reverse xs
