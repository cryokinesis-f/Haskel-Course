increase3 :: [Maybe Int] -> [Maybe Int]
increase3 x = ((+3) <$>) <$> x

increase3' :: [Maybe Int] -> [Maybe Int]
increase3' x = map f x where f n = case n of
                                         Just n  -> Just (n + 3)
                                         Nothing -> Nothing
