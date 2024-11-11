import Data.Fixed (mod')
mySin :: Double -> Double -> Double
mySin x eps = taylorSin (x `mod'` (2 * pi)) eps
  where
    taylorSin x eps = go x x 1
      where
        go term sum k
          | abs term < eps = sum -- если текущий член меньше eps, возвращаем результат
          | otherwise = go nextTerm (sum + nextTerm) (k + 1)
          where
            nextTerm = (-term * x * x) / (2 * k * (2 * k + 1))  -- вычисляем следующий член ряда


-- eps * (truncate (sum / eps))
-- a - b * fromIntegral (floor (a / b))
