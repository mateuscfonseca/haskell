import Data.List
-- ================================
-- EX 1
-- ================================
length' :: [a] -> Integer
length' [] = 0
length' xs = 1 + length' (tail xs)

-- ================================
-- EX 3
-- ================================
mean :: [Integer] -> Double
mean []     = 0.0
mean xs     = sum' / size
              where sum' = fromIntegral (sum xs)
                    size = fromIntegral (length' xs)

-- ================================
-- EX 4
-- ================================
toPal :: [a] -> [a]
toPal [] = []
toPal xs = xs ++ reverse xs

-- ================================
-- EX 5
-- ================================
isPal :: Eq a => [a] -> Bool
isPal [] = True
isPal xs = xs == (reverse xs)

-- ================================
-- EX 6
-- ================================
-- Sort list of lists by sub-lists size
sort' :: (Foldable t, Ord (t a)) => [t a] -> [t a]
sort' xs = xs'
           where xs'    = map snd sorted
                 sorted = sort pairs
                 pairs  = zip sizes xs
                 sizes  = map length xs

sort'' xs = map sort (sort' xs)
sortr  xs = reverse (sort'' xs)
