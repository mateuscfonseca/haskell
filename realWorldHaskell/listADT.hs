data List a = Add a (List a)
              | Nil
                deriving (Show)

fromList :: List a -> [a]
fromList (Add a b) = fromList b ++ [a]
fromList (Nil) = []
