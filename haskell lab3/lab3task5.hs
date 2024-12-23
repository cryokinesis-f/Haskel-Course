sumSerie :: (Int -> Double) -> Double -> Double
sumSerie a eps = sum $ takeWhile (\x -> abs x >= eps) $ zipWith (*) signs terms
  where
    terms = map a [1..]  -- Генерация последовательности элементов ряда
    signs = cycle [1, -1]  -- Чередующиеся знаки

{-map a [1..]: генерирует последовательность значений элементов ряда с использованием функции a для каждого индекса n, начиная с 1 и дальше
cycle [1, -1]: генерирует бесконечный список чередующихся знаков
zipWith (*) signs terms: объединяет элементы знаков и элементов ряда в новый список, умножая каждый элемент на соответствующий знак.
takeWhile (\x -> abs x >= eps): берет только те элементы ряда, которые имеют абсолютную величину больше или равную заданной точности 
sum: Суммирует эти элементы.-}