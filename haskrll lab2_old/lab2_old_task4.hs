intersectSegs :: (Ord a, Num a) => (a,a) -> (a,a) -> (Bool,(a,a))
intersectSegs (a1, a2) (b1, b2) =
    if max a1 b1 <= min a2 b2 
        then (True, (max a1 b1, min a2 b2))
        else (False,(0,0)) 