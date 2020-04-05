safeSecond :: [a] -> Maybe a

-- safeSecond [] = Nothing
-- safeSecond xs = if null (tail xs)
--                 then Nothing
--                 else Just (head (tail xs))
safeSecond (_:x:_) = Just x
safeSecond _ = Nothing
