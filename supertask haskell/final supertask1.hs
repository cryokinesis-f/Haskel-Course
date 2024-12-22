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

{-
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
-}

type GameState = ([Card], [Card])  -- (оставшиеся карты, накопитель) тип состояния

playGame :: [Card] -> Bool
playGame deck = 
    case find isFinal (iterate step (deck, [])) of
        Just ([_, _], []) -> True   -- Удачный исход
        _                 -> False -- Неудачный исход
  where
    -- Шаг игры, обновляем состояния
    step :: GameState -> GameState
    step (x:y:z:rest, acc)
      | matches x z =
          case acc of
            (a:b:as) -> (b:a:y:z:rest, as)  -- Берём две карты из накопителя
            (a:as)   -> (a:y:z:rest, as)  -- Берём одну карту из накопителя (если две нельзя)
            []       -> (y:z:rest, [])    -- Если накопитель пуст
      | otherwise = (y:z:rest, x:acc)    -- Добавляем левую карту в накопитель
    step state = state  -- Если карт меньше трёх, состояние не меняется

    -- Проверка финального состояния
    isFinal :: GameState -> Bool
    isFinal ([_, _], _) = True  -- Остаются две карты (накопитель неважен)
    isFinal _           = False




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

