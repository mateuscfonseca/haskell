repeatInArray x 0 = []
repeatInArray x y = do
           [x] ++ repeatInArray x (y-1)
f1 :: Int -> [Int] -> [Int]
f1 n [] = []
f1 n (x : arr) = do
                 repeatInArray x n ++ f1 n (tail arr)

f2 n = take n (repeat (take n (repeat [n])))
f2' n = take n (repeat (f2 n))
f2'' 0 = []
f2'' n = [n - 1] ++ (f2'' (n-1))

f3 [] = []
f3 l = [last l] ++ f3 (init l)

f4 arr = foldr (+) 0 ([x | x <- arr, odd x])

len :: [a] -> Int
len lst = foldr (+) 0 ([1 | x <- lst])

f5 arr = [(abs x) | x <- arr]

factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x-1)

expand :: Double -> Double
expand x = 1 + x + sum ([x**y / factorial y | (x, y) <- zip (take 8 (repeat x)) [1..x]])

f :: [Double] -> [Double]
f (x:[]) = [expand x]
f (x:xs) = [expand x] ++ f (tail xs)

readDouble :: Double
readDouble = readDouble :: Double

getDouble = do readDouble

getUserInput 0 = []
getUserInput n = [getDouble]
--
--
-- -- main = do
-- --        n <- readLn :: IO Int
-- --        print (f (take n (cycle[getUserInput])))
--
-- main = print getUserInput
call n = getUserInput n
main = print 1
