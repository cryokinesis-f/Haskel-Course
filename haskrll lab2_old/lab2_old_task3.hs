first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

triple :: a -> b -> c -> (a,b,c)
triple a b c = (a,b,c)

increaseElement :: Num a => (a,a,a) -> (Int,a) -> (a,a,a)
increaseElement troyka pair = -- (a,b,c) (index,value) troyka pair
    if (fst pair) > 3 || (fst pair) < 1 
        then error "Ошибка: неверно заданный индекс!"
        else if (fst pair) == 1
            then triple (first troyka + snd pair) (second troyka) (third troyka)
            else if (fst pair) == 2
                then triple (first troyka) (second troyka + snd pair) (third troyka)
                else triple (first troyka) (second troyka) (third troyka + snd pair)