primeDiv :: Int -> (Int, Int)
primeDiv x = 
  let iter state x k currentRes finalRes
        | state == 0 = if x == 1
                       then finalRes
                       else if (x `mod` k) == 0
                            then iter 1 (x `quot` k) k (k, 1) finalRes
                            else iter 0 x (k + 1) currentRes finalRes
        | state == 1 = if (x `mod` k) == 0
                       then iter 1 (x `quot` k) k (fst currentRes, (snd currentRes) + 1) finalRes
                       else if (x == 1) || (x `mod` k) > 0
                            then iter 0 x (k + 1) currentRes (if (snd currentRes) > (snd finalRes) then currentRes else finalRes)
                            else finalRes
  in iter 0 x 2 (0, 0) (0, 0)
