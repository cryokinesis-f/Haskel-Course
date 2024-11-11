gcd' :: Integer -> Integer -> Integer
gcd' a b
    | b == 0 = abs a
    | otherwise = gcd' b (a `mod` b)