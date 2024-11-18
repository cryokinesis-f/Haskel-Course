import Data.List (group, sort, maximumBy)
import Data.Ord (comparing)

-- Функция, которая возвращает пару (символ с максимальным количеством вхождений, строка без этого символа)
mostFrequent :: String -> (Char, String)
mostFrequent str = (mostCommonChar, filter (/= mostCommonChar) str)
  where
    -- Группируем и сортируем символы, чтобы определить их частоту
    groupedChars = group $ sort str
    -- Определяем символ с наибольшим количеством вхождений
    mostCommonChar = head $ maximumBy (comparing length) groupedChars


{-sort str сортирует строку по возрастанию, чтобы одинаковые символы были рядом.

group $ sort str создает список списков, где каждый подсписок содержит одинаковые символы (например, "aaabbc" → ["aaa", "bb", "c"]).

maximumBy (comparing length) groupedChars находит подсписок с максимальной длиной.

head берет первый символ из этого подсписка, который и есть наиболее часто встречающийся символ.
-}

