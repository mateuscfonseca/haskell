data Tree a = Node {
          value  :: a
          ,left  :: (Tree a)
          ,right :: (Tree a)
        }
        | Empty
          deriving (Show, Eq)

data List a = Cons a (List a)
              | Nil

type IntTree = (Tree Integer)
type StrTree = (Tree String)

-- sumt :: IntTree -> Integer
sumt Empty  = 0
sumt (Node value left right) = value + sumt left + sumt right
