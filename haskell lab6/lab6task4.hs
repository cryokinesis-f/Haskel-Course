import System.Random (randomRIO)
import Streaming.Prelude (untilLeft)

main :: IO ()
main = do
    putStrLn "Я загадал число от 1 до 1000000."
    secretNumber <- randomRIO (1, 1000000) :: IO Int
    guessNumber secretNumber

-- Основная функция для обработки предположений пользователя
guessNumber :: Int -> IO ()
guessNumber secret = do
    putStr "Ваше предположение: "
    guess <- readLn :: IO Int
    if guess < secret
        then do
            putStrLn "Загаданное число больше."
            guessNumber secret
        else if guess > secret
            then do
                putStrLn "Загаданное число меньше."
                guessNumber secret
            else putStrLn "Вы угадали число!"

guessNumber' :: Either () Int -> IO (Either () Int)
guessNumber' (Right secret) = do
    putStr "Ваше предположение: "
    guess <- readLn :: IO Int
    if guess < secret
        then do
            putStrLn "Загаданное число больше."
            return (Right secret)
        else if guess > secret
            then do
                putStrLn "Загаданное число меньше."
                return (Right secret)
            else do
                putStrLn "Вы угадали число!"
                return (Left ())
