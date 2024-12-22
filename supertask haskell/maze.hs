import System.Random
import Control.Monad (forM_)


type Cell = (Int, Int)
type Maze = (Cell, Cell)

-- Проверка, находится ли клетка внутри лабиринта
isValidCell :: (Int, Int) -> Cell -> Bool
isValidCell (n, m) (x, y) = x > 0 && x <= n && y > 0 && y <= m

-- Соседи клетки
neighbors :: (Int, Int) -> Cell -> [Cell]
neighbors (n, m) (x, y) =
  filter (isValidCell (n, m)) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Убираем стену между клетками
removeWall :: Cell -> Cell -> Maze -> Maze
removeWall c1 c2 maze = Set.insert (c1, c2) $ Set.insert (c2, c1) maze

-- Выбор случайного элемента из списка
randomChoice :: StdGen -> [a] -> (a, StdGen)
randomChoice gen xs = (xs !! idx, gen') where
  (idx, gen') = randomR (0, length xs - 1) gen

-- Алгоритм генерации лабиринта с возвратом
generateMaze :: Int -> Int -> StdGen -> Maze
generateMaze n m gen = go gen [(1, 1)] Set.empty Set.empty where
  go _ [] maze _ = maze
  go g (current:stack) maze visited
    | Set.size visited == n * m = maze
    | otherwise =
      let ns = filter (\cell -> not (Set.member cell visited)) (neighbors (n, m) current)
      in if null ns
         then go g stack maze visited
         else let (next, g') = randomChoice g ns
                  maze' = removeWall current next maze
                  visited' = Set.insert next visited
              in go g' (next:current:stack) maze' visited'

-- Рисование лабиринта
drawMaze :: Int -> Int -> Maze -> IO ()
drawMaze n m maze = do
  -- Рисуем верхнюю границу
  putStrLn $ concat [" _" | _ <- [1..m]]
  forM_ [1..n] $ \x -> do
    -- Рисуем левый край лабиринта
    putStr "|"
    forM_ [1..m] $ \y -> do
      let cell = (x, y)
          down = Set.member (cell, (x+1, y)) maze -- Есть ли проход вниз
          right = Set.member (cell, (x, y+1)) maze -- Есть ли проход вправо
      -- Если есть проход вниз, клетка пустая, иначе нижняя граница.
      putStr (if down then " " else "_")
      -- Для последней клетки строки рисуем правую стену, если прохода нет
      if y == m
        then putStrLn "|"  -- Для последней ячейки строки
        else putStr (if right then " " else "|")


-- Главная функция
main :: IO ()
main = do
  putStrLn "Введите высоту лабиринта:"
  n <- readLn
  putStrLn "Введите ширину лабиринта:"
  m <- readLn
  gen <- getStdGen
  let maze = generateMaze n m gen
  drawMaze n m maze


-- :set -package containers
-- :set -package mtl

