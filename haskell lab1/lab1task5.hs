powOf2 :: Integer -> Int
powOf2 n
  | n <= 0          = -1  -- число должно быть положительным
  | n == 1          = 0   -- 2^0 = 1
  | odd n  = -1  -- если n нечетное, это не степень двойки
  | otherwise       = findPower n 0
  where
    findPower x power
      | x == 1    = power
      | odd x = -1  -- если x не делится нацело на 2, не степень двойки
      | otherwise = findPower (x `div` 2) (power + 1)