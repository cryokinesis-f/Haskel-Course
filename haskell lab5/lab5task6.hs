import Control.Monad.Writer

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving Show

--без

search :: (Show a, Ord a) => Tree a -> a -> [String] -> (Bool, [String])
search EmptyTree _ path = (False, path ++ ["empty"])
search (Node x left right) current path
    | current == x = (True, path ++ ["found " ++ show x])
    | current < x = search left current (path ++ ["left"])
    | current > x = search right current (path ++ ["right"])

search'' :: (Show a, Ord a) => Tree a -> a -> (Bool, [String])
search'' tr findel = search tr findel ["start"]

--с

search' :: (Show a, Ord a) => Tree a -> a -> Writer [String] Bool
search' EmptyTree _ = do
    tell ["empty"]
    return False
search' (Node x left right) current
    | current == x = do
                tell ["found " ++ show x]
                return True
    | current < x = do
                    tell ["left"]
                    search' left current
    | current > x = do
                            tell ["right"]
                            search' right current


-- let tree = Node 10 (Node 5 EmptyTree EmptyTree) (Node 15 EmptyTree (Node 20 EmptyTree EmptyTree))

--ghci> search'' tree 20
--(True,["start","right","right","found 20"])

--ghci> snd (runWriter (search' tree 20))
--["right","right","found 20"]