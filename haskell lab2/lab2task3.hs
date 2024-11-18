import Data.List
posMax :: [String] -> String
posMax [] = []
posMax strs = map (\i -> maximum (map (!! i) strs)) [0..length (head strs) - 1]

-- "abc" "fgh" 