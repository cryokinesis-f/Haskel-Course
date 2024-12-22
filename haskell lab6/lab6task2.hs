import Data.List (sortOn)
import Data.Char (ord, chr)

main :: IO ()
main = do
    -- Считываем данные из файла
    content <- readFile "input.txt"
    let charCounts = countChars content
        sortedCounts = sortOn (negate . snd) charCounts
        result = map formatPair sortedCounts
    -- Записываем результат в файл
    writeFile "output.txt" (unlines result)

-- Функция для подсчёта символов
countChars :: String -> [(Char, Int)]
countChars content = [(c, count c content) | c <- ['\x20' .. '\x7e']] -- [chr 32 .. chr 127]

-- map (\l -> (head l, (subtract 1) . length $ l) $ group $ sort $ contents ++ [chr 32 .. chr 127]
  where
    count char = length . filter (== char)

-- Форматирование пары для записи в файл
formatPair :: (Char, Int) -> String
formatPair (char, count) = "('" ++ [char] ++ "' :" ++ show count ++ ")"
