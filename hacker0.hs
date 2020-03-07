--factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

--expand :: Int -> Double -> Double
expand 0 _ = 1
expand n x = ((x ^ n) / (fromIntegral (factorial n))) + (expand (n - 1) x)

--expandList :: Int -> [Double] -> [Double]
expandList 0 _ = []
expandList n [] = []
expandList n (x:xs) = (expand 9 x) : expandList (n-1) xs

main :: IO()
main = do
    n <- readLn :: IO Int
    inputdata <- getContents
    let
        numbers = map read (lines inputdata) :: [Double]
    putStrLn . unlines $ (map show . expandList n) numbers
