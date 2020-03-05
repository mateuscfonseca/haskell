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
f2'' n = [n % ] ++ (f2'' (n-1)) 

main :: IO()
main = do 
       nRepeat <- readLn :: IO Int
       print (f2'' nRepeat)