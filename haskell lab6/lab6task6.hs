import System.IO (readFile)

-- Функция для выполнения программы движения робота
moveRobot :: Int -> Int -> String -> [(Int, Int)]
moveRobot n m commands = moveRobot' 0 0 commands [(0, 0)]
  where
    -- Вспомогательная рекурсивная функция
    moveRobot' :: Int -> Int -> String -> [(Int, Int)] -> [(Int, Int)]
    moveRobot' _ _ [] visited = visited  -- Если команды закончились, возвращаем посещённые клетки
    moveRobot' x y (c:cs) visited
      | c == '>' && x + 1 < m = moveRobot' (x + 1) y cs ((x + 1, y) : visited)
      | c == '<' && x - 1 >= 0 = moveRobot' (x - 1) y cs ((x - 1, y) : visited)
      | c == 'v' && y + 1 < n = moveRobot' x (y + 1) cs ((x, y + 1) : visited)
      | c == '^' && y - 1 >= 0 = moveRobot' x (y - 1) cs ((x, y - 1) : visited)
      | otherwise = moveRobot' x y cs visited  -- Если выход за пределы, не двигаемся

-- Функция для создания поля 
printField :: Int -> Int -> [(Int, Int)] -> IO ()
printField n m visited = do
  let grid = [ [if (x, y) `elem` visited then '#' else '.' | x <- [0..m-1]] | y <- [0..n-1]]
  -- Выводим каждую строку поля
  mapM_ putStrLn (map (concatMap (:[])) grid)

-- Главная функция
main :: IO ()
main = do
  -- Чтение входных данных
  contents <- readFile "input.txt"
  let (sizeLine:programLine:_) = lines contents
      [n, m] = map read (words sizeLine)
      program = programLine

  -- Выполнение движения робота и вывод результата
  let visited = moveRobot n m program
  printField n m visited



