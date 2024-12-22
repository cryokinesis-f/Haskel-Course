import Data.Tuple
--без

updHeight :: (String, Int) -> Int -> (String, Int)
updHeight (name, height) increment = (name, height + increment)

updClassHeight :: [(String, Int)] -> Int -> [(String, Int)]
updClassHeight class_list increment = map (\student -> updHeight student increment) class_list

--с

updHeight' :: (String, Int) -> Int -> (String, Int)
updHeight' x increment = fmap (+ increment) x

updClassHeight' :: [(String, Int)] -> Int -> [(String, Int)]
updClassHeight' class_list increment = fmap (\student -> updHeight' student increment) class_list


--для инверсии
--без

updHeightR :: (Int, String) -> Int -> (Int, String)
updHeightR student increment = swap $ updHeight (swap student) increment

updClassHeightR :: [(Int, String)] -> Int -> [(Int, String)]
updClassHeightR class_list increment = map (\student -> updHeightR student increment) class_list

--с

updHeightR' :: (Int, String) -> Int -> (Int, String)
updHeightR' student increment = swap $ updHeight' (swap student) increment

updClassHeightR' :: [(Int, String)] -> Int -> [(Int, String)]
updClassHeightR' class_list increment = fmap swap (updClassHeight (fmap swap class_list) increment)



