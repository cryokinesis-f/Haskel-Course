maxRoot :: Int -> Int -> Int -> Double
maxRoot a b c
  | d < 0     = 0 / 0  -- NaN, если дискриминант отрицательный
  | d == 0    = -fromIntegral b / (2 * fromIntegral a)  -- один корень
  | otherwise = max x1 x2  -- два корня, выбираем наибольший
  where
    d  = fromIntegral b ^ 2 - 4 * fromIntegral a * fromIntegral c
    x1 = (- fromIntegral b + sqrt d) / (2 * fromIntegral a)
    x2 = (- fromIntegral b - sqrt d) / (2 * fromIntegral a)