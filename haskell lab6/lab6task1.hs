import Data.List (sortOn)
import System.IO (readFile, writeFile)
import Data.Ord

main :: IO ()
main = do
    -- Считываем данные из файла
--    content <- readFile "input.txt"
    numbers <- (map read . words) <$> readFile "input.txt" :: IO [Int]
    let -- numbers = map read $ words content :: [Int]
        sorted = sortOn Down $ filter (>= 0) numbers -- Сортируем неотрицательные по убыванию
    -- Записываем результат в файл
    writeFile "output.txt" $ unwords $ map show sorted


-- down 