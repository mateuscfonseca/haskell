data Coisa = Chave
            | Carteira
            | Oculos
            | Mochila
            | Anel
    deriving Show

type Nome = String
type Idade = Int 
    
data Pessoa = Pessoa Nome Idade Coisa
    deriving Show

   
idade :: Pessoa -> Int
idade (Pessoa _ a _) = a

baz :: Pessoa -> String
baz p@(Pessoa n _ _) = show p ++ " field name " ++ n 





