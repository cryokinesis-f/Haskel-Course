--a f(x,y) = x^y
power :: Int -> Int -> Int
power x y = x ^ y  

power' :: Int -> Int -> Int
power' = (^)  -- просто передаем оператор возведения в степень как функцию

--б f(x,y) = (x+1)^y

power2 :: Int -> Int -> Int
power2 x y = (x + 1) ^ y  

power2' :: Int -> Int -> Int
power2' = (^) . (+ 1)

--в f(x,y) = x^(y−3)
power3 :: Int -> Int -> Int
power3 x y = x ^ (y - 3)

power3' :: Int -> Int -> Int
power3' = flip (flip (^) . (+ (-3)))

--г  f(x,y) = (x+ 1)^(y−3)
power4 :: Int -> Int -> Int
power4 x y = (x + 1) ^ (y - 3) 

power4' :: Int -> Int -> Int
power4' = flip (flip ((^) . (+ 1)) . (+ (-3)))