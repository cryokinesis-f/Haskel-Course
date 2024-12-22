import Control.Monad.Reader

--без

root :: (Double -> Double) -> Double -> Double -> Double -> Double
root func eps a b
    | b - a < eps = c
    | func a * func c < 0       = root func eps a c
    | otherwise           = root func eps c b
    where
        c = (a + b) / 2
--с

root' :: Double -> Double -> Reader (Double -> Double, Double) Double
root' a b = do
    (func, eps) <- ask
    let c = (a + b) / 2
    let funcc = func c
    if b - a < eps
       then return c
       else do
           if func a * funcc < 0
              then root' a c
              else root' c b

-- let func x = ...
-- runReader (root' 1 2) (\x -> x^2 - 2, 1e-6)