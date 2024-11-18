import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

-- Функция для вычисления полярного угла
polarAngle :: (Double, Double) -> Double
polarAngle (x, y) = let angle = atan2 y x in if angle < 0 then angle + 2 * pi else angle

-- Функция для сравнения 
comparePoints :: (Double, Double) -> (Double, Double) -> Ordering
comparePoints p1 p2
  | polarAngle p1 > polarAngle p2 = GT
  | polarAngle p1 < polarAngle p2 = LT
  | otherwise = compare (distanceSquared p2) (distanceSquared p1)  -- Если углы равны, сравниваем расстояния
  where
    distanceSquared (x, y) = x^2 + y^2  -- Функция для вычисления квадрата расстояния


-- Основная функция
minAngle :: [(Double, Double)] -> (Double, Double)
minAngle = minimumBy comparePoints