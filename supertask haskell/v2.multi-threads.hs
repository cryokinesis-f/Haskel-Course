import System.Random
import Control.Monad (replicateM)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent 


-- Типы карт
data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Show)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Show)
data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq, Show)

-- Колоды для 36 карт (без 2,3,4,5) и 52 карты (с 2,3,4,5)
getDeck :: Int -> [Card]
getDeck size
  | size == 36 = [Card suit rank | suit <- [Hearts, Diamonds, Clubs, Spades], rank <- [Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]]
  | size == 52 = [Card suit rank | suit <- [Hearts, Diamonds, Clubs, Spades], rank <- [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]]
  | otherwise = []

-- Проверка, совпадают ли две карты по масти или достоинству
matches :: Card -> Card -> Bool
matches (Card suit1 rank1) (Card suit2 rank2) = suit1 == suit2 || rank1 == rank2

-- Перемешивание колоды
shuffle :: [Card] -> IO [Card]
shuffle deck = do
  let n = length deck
  indices <- getUniqueIndices n  -- Генерация уникальных индексов
  return [deck !! i | i <- indices]

-- Генерация уникальных случайных индексов
getUniqueIndices :: Int -> IO [Int]
getUniqueIndices n = go n []  -- Начинаем с пустого списка для индексов
  where
    go 0 indices = return indices  -- Когда список индексов полон, возвращаем его
    go remaining indices = do
      idx <- randomRIO (0, n - 1)  -- Генерация случайного индекса
      if idx `elem` indices       -- Проверяем, не был ли этот индекс уже
        then go remaining indices  -- Если индекс уже есть, пробуем снова
        else go (remaining - 1) (idx : indices)  -- Добавляем новый индекс и продолжаем

-- Основной процесс игры 
playGame :: [Card] -> Bool
playGame deck = go deck []  -- Передаем накопитель для карт, которые остались слева
  where
    -- Если осталось 2 карты на столе, игра считается удачной
    go [x, y] [] = True
    -- Если в списке больше 2 карт, проверяем тройки и обрабатываем их
    go (x:y:z:rest) acc
      | matches x z = go (reverse acc ++ (y:z:rest)) []  -- Если совпали крайние карты, убираем левую и начинаем с нового состояния
      | otherwise   = go (y:z:rest) (x:acc)  -- Иначе сдвигаем окно и добавляем левую карту в начало накопителя
    -- Если не осталось троек, а на столе больше двух карт, игра неудачная
    go _ _ = False

-- Метод Монте-Карло 
monteCarlo :: Int -> Int -> IO Double
monteCarlo trials size = do
  numCapabilities <- getNumCapabilities  -- Получаем количество доступных ядер
  let chunkSize = trials `div` numCapabilities  -- Разбиваем задачи на части
  -- Параллельная обработка
  tasks <- mapM (\i -> async (processChunk (i * chunkSize) chunkSize size)) [0..numCapabilities-1]
  results <- mapM wait tasks  -- Ждем завершения всех задач
  let successCount = length (filter id results)
  return $ fromIntegral successCount / fromIntegral trials
  --return $ fromIntegral successCount


-- Обработка части задач
processChunk :: Int -> Int -> Int -> IO Bool
processChunk start count size = do
  decks <- replicateM count (shuffle (getDeck size))
  let results = map playGame decks
  return $ any id results  -- Если хотя бы одна игра успешна

-- Симуляция игры
simulate :: [Card] -> IO Bool
simulate shuffledDeck = return $ playGame shuffledDeck

--4 ядра 45 секунд
--повторный запуск дал 32



shuffle' :: [a] -> IO [a]
shuffle' deck = do
  gen <- newStdGen  -- Генерация нового генератора случайных чисел
  return $ fisherYates gen deck

fisherYates :: RandomGen g => g -> [a] -> [a]
fisherYates _ [] = []  -- Если список пуст, возвращаем пустой список
fisherYates gen xs = runFisher gen xs (length xs - 1)
  where
    runFisher _ [] _ = []  -- Когда список пуст, возвращаем пустой список
    runFisher g lst i
      | i == 0 = lst  -- Когда остаётся один элемент, возвращаем текущий список
      | otherwise =
          let (j, g') = randomR (0, i) g  -- Генерация случайного индекса в пределах текущего диапазона
              (l1, x:l2) = splitAt j lst  -- Разделяем список на две части
              newList = l1 ++ [lst !! i] ++ l2  -- Переставляем элементы
          in runFisher g' newList (i - 1)  -- Рекурсивно вызываем функцию для оставшегося списка


playGame' :: [Card] -> Bool
playGame' deck = go deck []  -- Передаем накопитель для карт, которые остались слева
  where
    -- Если осталось 2 карты на столе, игра считается удачной
    go [x, y] [] = True
    -- Если в списке больше 2 карт, проверяем тройки и обрабатываем их
    go (x:y:z:rest) acc
      | matches x z = go (reverse acc ++ (y:z:rest)) []  -- Если совпали крайние карты, убираем левую и начинаем с нового состояния
      | otherwise   = go (y:z:rest) (x:acc)  -- Иначе сдвигаем окно и добавляем левую карту в начало накопителя
    -- Если не осталось троек, а на столе больше двух карт, игра неудачная
    go _ _ = False

-- Симуляция игры
simulate' :: [Card] -> IO Bool
simulate' shuffledDeck = return $ playGame shuffledDeck



monteCarlo' :: Int -> Int -> IO Double
monteCarlo' trials size = do
  numCapabilities <- getNumCapabilities  -- Получаем количество доступных ядер
  let chunkSize = trials `div` numCapabilities  -- Разбиваем задачи на части
  -- Параллельная обработка
  tasks <- mapM (\i -> async (processChunk' (i * chunkSize) chunkSize size)) [0..numCapabilities-1]
  results <- mapM wait tasks  -- Ждем завершения всех задач
  let successCount = length (filter id results)
  return $ fromIntegral successCount / fromIntegral trials
  --return $ fromIntegral successCount

-- Обработка части задач
processChunk' :: Int -> Int -> Int -> IO Bool
processChunk' start count size = do
  decks <- replicateM count (shuffle' (getDeck size))
  let results = map playGame' decks
  return $ any id results  -- Если хотя бы одна игра успешна



{-
export GHCRTS="-N"
ghci -threaded
:l v2.multi-threads.hs
setNumCapabilities 4 до 270



ghci> monteCarlo trials 36
4.0e-6
1.23

ghci> monteCarlo' trials 36
4.0e-6
0.02

ghci> let trials = 1666787
ghci> monteCarlo' trials 36
2.399826732509913e-6
-}





testing_generator :: Int -> IO ()
testing_generator k = 
  if k == 10
    then putStrLn "Работа закончена"
    else do
      let trials = k
      probability <- monteCarlo' trials 36  
      putStrLn $ "Вероятность события: " ++ show probability
      testing_generator (k + 1)




testing_generator2 :: Int -> IO ()
testing_generator2 k = 
  if k == 10
    then putStrLn "Работа закончена"
    else do
      let trials = k
      probability <- monteCarlo trials 36  
      putStrLn $ "Вероятность события: " ++ show probability
      testing_generator (k + 1)



modified_test k =
  if k == 1000000
    then putStrLn "Работа закончена"
    else do
      let trials = k
      probability1 <- monteCarlo' trials 36  
      putStrLn $ "Вероятность события fast: " ++ show probability1
      probability2 <- monteCarlo trials 36  
      putStrLn $ "Вероятность события slow: " ++ show probability2
      modified_test (k + 1)
