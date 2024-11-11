isPrime :: Integer -> Bool
isPrime x
  | x < 2 = False
  | otherwise = iter 2
  where
    s = floor (sqrt (fromIntegral x))
    iter a
      | a > s = True
      | x `mod` a == 0 = False
      | otherwise = iter (a + 1)

      -- = a > s || (x `mod` a /= 0 && iter (a+1))