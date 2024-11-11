import Data.Fixed (mod')
gcd' :: Integer -> Integer -> Integer
gcd' a b
    | b == 0 = abs a
    | otherwise = gcd' b (a `mod` b)


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


maxRoot :: Int -> Int -> Int -> Double
maxRoot a b c
  | d < 0     = 0 / 0  -- NaN, если дискриминант отрицательный
  | d == 0    = -fromIntegral b / (2 * fromIntegral a)  -- один корень
  | otherwise = max x1 x2  -- два корня, выбираем наибольший
  where
    d  = fromIntegral b ^ 2 - 4 * fromIntegral a * fromIntegral c
    x1 = (- fromIntegral b + sqrt d) / (2 * fromIntegral a)
    x2 = (- fromIntegral b - sqrt d) / (2 * fromIntegral a)


root :: (Double -> Double) -> Double -> Double -> Double -> Double
root f a b eps
  | abs (b - a) < eps = (a + b) / 2  -- Если интервал достаточно мал, возвращаем середину
  | f a * f c <= 0    = root f a c eps  -- Корень на левом интервале
  | otherwise         = root f c b eps  -- Корень на правом интервале
  where
    c = (a + b) / 2  -- Середина интервала


powOf2 :: Integer -> Int
powOf2 n
  | n <= 0          = -1  -- Число должно быть положительным
  | n == 1          = 0   -- 2^0 = 1
  | odd n  = -1  -- Если n нечетное, это не степень двойки
  | otherwise       = findPower n 0
  where
    findPower x power
      | x == 1    = power
      | odd x = -1  -- Если x не делится нацело на 2, не степень двойки
      | otherwise = findPower (x `div` 2) (power + 1)


mySin :: Double -> Double -> Double
mySin x eps = taylorSin (x `mod'` (2 * pi)) eps
  where
    taylorSin x eps = go x x 1
      where
        go term sum k
          | abs term < eps = sum  -- Если текущий член меньше eps, возвращаем результат
          | otherwise = go nextTerm (sum + nextTerm) (k + 1)
          where
            nextTerm = (-term * x * x) / (2 * k * (2 * k + 1))  -- Вычисляем следующий член ряда


main :: IO ()
main = do
    print (gcd' 0 (-6))
    print (isPrime 7)
    print (isPrime 1)
    print (isPrime 2)
    print (gcd' 3 7)
    print (maxRoot 1 20 40)
    let f x = x^2 - 2  -- Функция с корнем в sqrt(2)
    print (root f 1 23 0.0001)  -- Ожидается приближённое значение корня sqrt(2)
    print (powOf2 1023)
    let x = pi / 4  -- угол в радианах
    let eps = 0.001  -- требуемая точность
    print (mySin x eps)  -- Ожидается значение, близкое к sin(pi/4) ≈ 0.7071