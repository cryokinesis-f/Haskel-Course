import System.Random (randomRIO)
import Data.List 
import Control.Monad

-- Количество дней в каждом месяце (невисокосный год)
daysInMonth :: Int -> Int
daysInMonth month
  | month == 2 = 28
  | month == 4 || month == 6 || month == 9 || month == 11 = 30
  | otherwise = 31

-- Функция для генерации случайной даты рождения
generateBirthday :: IO (Int, Int)
generateBirthday = do
  month <- randomRIO (1, 12)  -- Месяц от 1 до 12
  day <- randomRIO (1, daysInMonth month)  -- День в месяце
  return (day, month)

-- Функция для генерации набора из n дат рождения
generateBirthdays :: Int -> IO [(Int, Int)]
generateBirthdays n = replicateM n generateBirthday --mapM (const generateBirthday) [1..n]

-- Функция для проверки наличия дубликатов
hasDuplicates :: (Eq a, Ord a) => [a] -> Bool
hasDuplicates xs = all ((== 1) . length) $ group $ sort $ xs --length xs /= length (nub xs)

-- Основная функция, которая проверяет, сколько раз в m испытаниях
-- найдены хотя бы два человека с одинаковыми датами рождения
birthday :: Int -> Int -> IO Double
birthday trials groupSize = do
  results <- replicateM trials (checkForDuplicates groupSize) 
  return $ fromIntegral (length (filter id results)) / fromIntegral trials
  where
    checkForDuplicates :: Int -> IO Bool
    checkForDuplicates groupSize = hasDuplicates <$> generateBirthdays groupSize

-- Пример использования: провести 10000 испытаний для группы из 23 человек
main :: IO ()
main = do
  putStr "Введите число попыток"
  input <- getLine
  let trials = read input :: Int
  let groupSize = 23
  probability <- birthday trials groupSize
  putStrLn $ "Вероятность события, что хотя бы два человека имеют одинаковый день рождения: " ++ show probability
