import Data.List (find)
gtMill :: Integer -> Integer
gtMill k = case find (\n -> abs (fromIntegral n / sin (fromIntegral n ^ 2)) > 10^6) [k..] of
    Just n  -> n
    Nothing -> error "No solution found"



gtMill' :: Integer -> Integer
gtMill' k = head $ dropWhile (\n -> abs (fromIntegral n / sin (fromIntegral n ^ 2)) < 10^6) [k..] 
{-[k..]:
cоздаётся бесконечный список натуральных чисел, начиная с k.
Функция find ищет первое число n в списке, которое удовлетворяет условию:

Условие проверяется с помощью лямбда-функции \n -> ....
Результат:
Если такое число найдено, оно возвращается-}