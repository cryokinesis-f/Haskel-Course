main :: IO ()
main = do
    putStrLn "Загадайте число от 1 до 1000000."
    guessNumber 1 1000000

-- Основная функция для угадывания числа
guessNumber :: Int -> Int -> IO ()
guessNumber low high
    | low > high = putStrLn "Ошибка"
    | otherwise = do
        let mid = (low + high) `div` 2
        putStr $ "Ваше число " ++ show mid ++ "? (меньше - 'l', больше - 'h', угадал - 'y'): "
        response <- getChar
        getChar -- Пропустить символ новой строки
        case response of
            'l' -> guessNumber low (mid - 1) -- Число меньше
            'h' -> guessNumber (mid + 1) high -- Число больше
            'y' -> putStrLn $ "Я угадал ваше число: " ++ show mid
            _   -> do
                putStrLn "Введите 'l', 'h' или 'y'."
                guessNumber low high
