import Data.List (sort)

-- Функция для нахождения медианы списка
median :: (Fractional a, Ord a) => [a] -> a
median xs = 
  let sorted = sort xs
      n = length xs
  in if odd n
     then sorted !! (n `div` 2)
     else (sorted !! (n `div` 2 - 1) + sorted !! (n `div` 2)) / 2

-- Функция, возвращающая элементы списка, которые строго больше медианы
upperHalf :: (Fractional a, Ord a) => [a] -> [a]
upperHalf xs = 
  let m = median xs
  in filter (> m) xs
