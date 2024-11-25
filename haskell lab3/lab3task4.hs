root :: (Double -> Double) -> Double -> Double -> Double -> Double
root f a b eps = fst $ head $ filter (\(x, y) -> abs (y - x) < eps) $ iterate step (a, b)
  where
    step (x, y) = let c = (x + y) / 2 in if f x * f c <= 0 then (x, c) else (c, y). 