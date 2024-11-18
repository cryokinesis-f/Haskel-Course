-- head, tail, null, :
indexing :: [a] -> [(Int, a)]
indexing lst = iter lst 1 []
  where
    iter [] _ res = res
    iter (x:xs) k res = iter xs (k + 1) (res ++ [(k, x)])


indexing' :: [a] -> [(Int, a)]
indexing' lst = zip [1..length lst] lst