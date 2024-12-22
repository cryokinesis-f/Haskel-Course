import System.Random
import Control.Monad
import Data.List
import System.Random.Shuffle.FisherYates

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

--под playGame описание алгоритма 

playGame :: [Card] -> Bool
playGame deck = go deck []  -- Начинаем с пустого накопителя
  where
    -- Если осталось 2 карты на столе, игра считается удачной
    go [x, y] [] = True
    go [x, y] _  = False  -- Если накопитель не пуст, а карты кончились, то игра неудачная
    -- Если в списке больше 2 карт, проверяем тройки и обрабатываем их
    go (x:y:z:rest) acc
      | matches x z =
          case acc of
            (a:b:as) -> go (a:b:y:rest) as  -- Берем верхнюю карту из накопителя и продолжаем
            (a:as) -> go (a:y:z:rest) as
            []     -> go (y:z:rest) []    -- Если накопитель пуст, продолжаем с текущим списком
      | otherwise = go (y:z:rest) (x:acc)  -- Иначе добавляем левую карту в накопитель
    -- Если карты закончились, а тройки больше не образуются, игра неудачная
    go _ _ = False

{-
как работает вместо reverse
теперь, если тройка нашлась, то мы не разворачиваем список, а берем верхнюю карту, если она есть из накопителя acc: (a:as) -> go (a:y:z:rest) as
начинаем обрабатывать наш список с двум оставшимися карты тройки и картой, которая была перед ними

пример:
acc содержит карты  (Король Пик), (Дама Червей), (6 крести)
у нас только что сложилась тройка, мы оставили две карты (5 крести) (9 крести) (оставшиеся карты)
тогда в новую итерацию мы пойдем с (Король Пик) (5 крести) (9 крести)
если тройка сложилась, то мы повторяем заново
если не сложилась, то короля мы отправляем обратно в накопитель, и берем (5 крести) (9 крести) и следующую карту колоды

если бы накопитель был пуст, то мы просто берем (5 крести) (9 крести) и следующую карту колоды

добавил конечное состояние, когда накопитель полон
в него мы можем попасть только если попали в ветвь, где тройка не сложилась
-}


-- Метод Монте-Карло 
monteCarlo :: Int -> Int -> IO Double
monteCarlo trials size = do
  results <- replicateM trials (simulate size)
  let successCount = length (filter id results)
  return $ fromIntegral successCount / fromIntegral trials
  --return $ fromIntegral successCount -- Для отладки

-- Симуляция игры
simulate :: Int -> IO Bool
simulate size = do
  let deck = getDeck size
  shuffledDeck <- shuffle deck
  return $ playGame shuffledDeck

