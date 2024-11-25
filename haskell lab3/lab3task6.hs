allDigits :: Integer -> [Int]
allDigits n = concatMap digits [1..n]
  where
    digits x = go x []
    go 0 acc = acc
    go x acc = go (x `div` 10) (fromIntegral (x `mod` 10) : acc)

{-
concatMap digits [1..n]: Мы используем concatMap, чтобы для каждого числа от 1 до n применить функцию digits, которая возвращает список цифр этого числа. Все такие списки цифр будут объединены в один.
-}

allDigits' :: Integer -> [Int]
allDigits' n = concatMap digits [1..fromIntegral n]
  where
    digits x = reverse $ takeWhile (> 0) $ map (`mod` 10) $ iterate (`div` 10) x

{-
ghci
:l lab3task6.hs
f n = snd $ head $ dropWhile ((> 0) . fst) $ iterate (\(k,l) -> (k `div` 10, k `mod` 10 : l)) (n,[])


ghci> f 10
[1,0]
ghci> concatMap f [1..10]
[1,2,3,4,5,6,7,8,9,1,0]
ghci> concatMap f [1..11]
[1,2,3,4,5,6,7,8,9,1,0,1,1]
ghci> concatMap f [1..23]
[1,2,3,4,5,6,7,8,9,1,0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1,8,1,9,2,0,2,1,2,2,2,3]
-}