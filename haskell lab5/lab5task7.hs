import Control.Monad.State
import Control.Monad

--без

firstDigits :: [Int] -> [Int]
firstDigits nums = foldl (\counts num -> let digit = firstDigit num in updateCount digit counts) (replicate 9 0) nums
    where
        firstDigit n = read [head (show n)] :: Int
        updateCount digit counts = take (digit - 1) counts ++ [counts !! (digit - 1) + 1] ++ drop digit counts


--с

type CountState = State [Int] ()

-- Функция, извлекающая первую цифру числа
firstDigit :: Int -> Int
firstDigit n = read [head (show n)] :: Int

-- Функция для обновления состояния, когда встретили число, начинающееся с цифры
updateCount :: Int -> CountState
updateCount digit = do
    counts <- get
    let updatedCounts = take (digit - 1) counts ++ [counts !! (digit - 1) + 1] ++ drop digit counts
    put updatedCounts

-- Основная функция для подсчета чисел, начинающихся с каждой цифры 1-9
firstDigits' :: [Int] -> [Int]
firstDigits' nums = execState (foldM_ (\_ num -> updateCount (firstDigit num)) () nums) (replicate 9 0)
