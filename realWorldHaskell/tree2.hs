data Tree a = Node
              (Maybe a)
              (Maybe (Tree a))
              (Maybe (Tree a))
          deriving (Show, Eq)

type IntTree = (Tree Integer)
type StrTree = (Tree String)

may may = case may of
           Just v -> v
           Nothing -> Nothing

sumt tree = case tree of
              Node v l r -> (may v) + (sumt (may l)) + (sumt (may r))
