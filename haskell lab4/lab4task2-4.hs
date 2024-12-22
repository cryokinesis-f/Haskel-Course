--task2
--a

class Monoid' a where
    (.+.) :: a -> a -> a
    zero :: a

--б

class Monoid' a => Group a where
    opposite :: a -> a
    (.-.) :: a -> a -> a

    opposite x = zero .-. x
    x .-. y = x .+. opposite y

    {-# MINIMAL opposite | (.-.) #-}


--в

instance Monoid' Double where
    (.+.) = (+)
    zero = 0

instance Group Double where
    opposite x = -x
    x .-. y = x + (opposite y)

--г

instance (Monoid' b) => Monoid' (a->b) where
    (.+.) f g = \x -> (.+.) (f x) (g x)
    zero _ = zero

instance (Group b) => Group (a->b) where
    opposite f = \x -> opposite (f x)
    f .-. g = \x -> (f x) .-. (g x)


--task3
--a

class Group a => LinearSpace a where
  (.*.) :: Double -> a -> a

--б

data Vector a = ZeroVector           -- Нулевой вектор
              | Vector [a]           -- Вектор с компонентами типа `a`
  deriving (Eq, Show)


--в

instance (Monoid' a) => Monoid' (Vector a) where
    (.+.) x ZeroVector = x
    (.+.) ZeroVector x = x
    (.+.) (Vector x) (Vector y) = Vector (zipWith (.+.) x y)
    zero = ZeroVector

instance (Group a) => Group (Vector a) where
    opposite ZeroVector = ZeroVector
    opposite (Vector x) = Vector (map opposite x)
    (.-.)  (Vector x) (Vector y) = Vector (zipWith (.-.) x y)

instance (LinearSpace a) => LinearSpace (Vector a) where
    (.*.) _ ZeroVector = ZeroVector
    (.*.) scalar (Vector y) = Vector (map (\x -> (.*.) scalar x) y)

--г

instance (LinearSpace b) => LinearSpace (a -> b) where
    (.*.) scalar f = \x -> (.*.) scalar (f x)
--
instance LinearSpace Double where
    (.*.) scalar x = (*) scalar x



--task4

--a

class LinearSpace a => HilbertSpace a where
  (%) :: a -> a -> Double

--б

instance HilbertSpace (Vector Double) where
    (%) ZeroVector _ = 0
    (%) _ ZeroVector = 0
    (%) (Vector x) (Vector y) = sum (zipWith (*) x y)

--в


instance HilbertSpace (Double -> Double) where
    (%) f g = lebeg (\x -> (f x) * (g x)) where
                                             lebeg :: (Double -> Double) -> Double
                                             lebeg func = h * sum [func (i * h) | i <- [1.. 100000]] where h = 1/100000