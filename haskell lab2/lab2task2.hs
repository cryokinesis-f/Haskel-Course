separate3 :: [Int] -> ([Int], [Int], [Int])
separate3 lst = iter lst [] [] []
  where
    iter [] res0 res1 res2 = (reverse res0, reverse res1, reverse res2)
    iter (x:xs) res0 res1 res2
        | x `mod` 3 == 0 = iter xs (x : res0) res1 res2
        | x `mod` 3 == 1 = iter xs res0 (x : res1) res2
        | x `mod` 3 == 2 = iter xs res0 res1 (x : res2)

