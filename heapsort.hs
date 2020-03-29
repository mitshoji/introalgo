data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a) deriving (Show)

buildTree :: Ord a => [a] -> Tree a
buildTree = foldr insertTree Empty

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x Empty = Leaf x
insertTree x (Leaf y)
    | x <= y = Node y (Leaf x) Empty
    | x > y  = Node x (Leaf y) Empty  
insertTree x (Node y left right)
    | x <= y = selectBranch x (Node y left right)
    | x > y  = Node x left (insertTree y right)


selectBranch :: Ord a => a -> Tree a -> Tree a 
selectBranch v (Node n Empty trr)
                       | v < val trr = Node n (Leaf v) trr
                       | otherwise   = Node n trr (Leaf v)
selectBranch v (Node n trl Empty)
                       | v < val trl = Node n (Leaf v) trl
                       | otherwise   = Node n trl (Leaf v)
selectBranch v (Node n trl trr)
                       | v < val trr = Node n (insertTree v trl) trr
                       | val trl < v = Node n trl (insertTree v trr)

val :: Ord a => Tree a -> a
--val Empty      = 
val (Leaf x)       = x 
val (Node x _ _ )  = x
