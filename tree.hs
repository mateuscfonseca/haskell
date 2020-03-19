data Tree a = Node {
          value  :: a
          ,left  :: (Tree a)
          ,right :: (Tree a)
        }
        | Empty
          deriving (Show, Eq)

type IntTree = (Tree Integer)
type StrTree = (Tree String)

-- sumt :: IntTree -> Integer
sumt Empty                   = 0
sumt (Node value left right) = value + sumt left + sumt right

concatt Empty                   = ""
concatt (Node value left right) = value ++ concatt left ++ concatt right
