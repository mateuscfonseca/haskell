-- =====================================================================================
-- 	Variaveis Locais
-- =====================================================================================


-- ===================================================
-- 	USING let AND in
-- ===================================================
lend amount balance = let reserve    = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance

-- ===================================================
-- 	USING where
-- ===================================================
lend' amount balance = if amount > reserve * 0.5
                  then Nothing
                  else Just newBalance
                  where reserve = 100
                        newBalance    = balance - amount
-- ===================================================
-- 	USING GUARDS ( | )
-- ===================================================
lend'' amount balance
                     | amount <= 0            = Nothing
                     | amount > reserve * 0.5 = Nothing
                     | otherwise              = Just newBalance
                     where  reserve    = 100
                            newBalance = balance - amount
