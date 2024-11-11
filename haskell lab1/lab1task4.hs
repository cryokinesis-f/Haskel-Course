root :: (Double -> Double) -> Double -> Double -> Double -> Double
root f a b eps
  | abs (b - a) < eps = (a + b) / 2  -- если интервал мал, возвращаем середину
  | f a * f c <= 0    = root f a c eps  -- корень на левом интервале
  | otherwise         = root f c b eps  -- корень на правом интервале
  where
    c = (a + b) / 2  -- середина интервала