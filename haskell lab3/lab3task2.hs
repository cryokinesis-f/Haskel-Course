import Data.List (find)

excludeDivisor :: Integer -> Integer -> Integer
excludeDivisor n k
    | k <= 1    = error "k должно быть больше 1"
    | otherwise = head $ dropWhile (\q -> q `mod` k == 0) $ iterate (`div` k) n


--[n, n `div` k .. 1]

=