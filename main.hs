{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

import System.IO
hlwl 0 = putStr ""
hlwl x =  do 
          putStrLn "Hello World"
          hlwl (x-1)	
main :: IO()
main = do 
       n <- readLn :: IO Int
       hlwl(n)	
