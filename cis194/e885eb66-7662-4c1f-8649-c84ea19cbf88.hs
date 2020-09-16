data Coisa = Chave
            | Carteira
            | Oculos
            | Mochila
            | Anel
    deriving Show

    
    type Nome = String
    type Idade = Int
    type Coisa = 
        
        
        getAge :: Person -> Int
        getAge (Person _ a _) = a
        
        baz :: Person -> String
        baz p@(Person n _ _) = show p ++ " field name " ++ n 





