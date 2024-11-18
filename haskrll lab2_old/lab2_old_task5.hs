-- ("0", "1")
bits :: Int -> (Int, Int)
bits x = 
    if x == 0 
        then (1,0)
        else if x == 1
            then (0,1)
            else
    let iter y res
            | y == 0 = res
            | y `mod` 2 == 0 = iter (y `quot` 2) (fst res + 1, snd res)
            | y `mod` 2 == 1 = iter (y `quot` 2) (fst res, snd res + 1)
    in iter x (0, 0)
