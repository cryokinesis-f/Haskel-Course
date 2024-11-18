equalPair :: Eq a => (a,a) -> Bool
equalPair (a,b) = 
    if fst (a,b) == snd (a,b)
        then True
        else False



-- или 


helper :: Int -> Int -> Bool
helper a b =
    if a == b 
        then True
        else False

equalPair2 :: (Int, Int) -> Bool
equalPair2 = uncurry helper