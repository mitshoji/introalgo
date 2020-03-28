maxsubarray_old :: [Int] -> [[Int]]
maxsubarray_old [] = []
maxsubarray_old (x:xs) = [take n (x:xs) | n <- [1..length (x:xs)]] ++ maxsubarray_old xs

findMaxsubarray_old :: [Int] -> (Int, [Int])
findMaxsubarray_old xs = maximum $ zip (map sum ns) ns
    where
       ns = maxsubarray_old xs

--------------------------------------------------------------------------------------------
findMaxsubarray :: [Int] -> (Int, [Int])
findMaxsubarray [x] = (x,[x])
findMaxsubarray xs = maximum [findMaxsubarray ys, maxMiddle xs, findMaxsubarray zs]
    where
       (ys, zs) = (take nhalf xs, drop nhalf xs)
       nhalf = (length xs) `div` 2

maxMiddle :: [Int] -> (Int, [Int])
maxMiddle [x] = (x, [x])
maxMiddle xs = maxSumup ys `tapplesum` maxSumup zs
    where
       (ys, zs) = (reverse $ take nhalf xs, drop nhalf xs)
       nhalf = (length xs) `div` 2

tapplesum :: (Int, [Int]) -> (Int, [Int]) -> (Int, [Int])
tapplesum (x,xs) (y, ys) = (x+y, xs `tps` ys)
    where
       tps [] ys     = ys
       tps (x:xs) ys = tps xs (x:ys)

maxSumup :: [Int] -> (Int, [Int])
maxSumup xs = maximum $ zip (map sum ns) ns
    where
       ns = [take n xs | n <- [1..length xs]]


array :: [Int]
array = [-3, -7, 6, -1, -1, 1, -1, -7, 10, 9, -1, -9, -3, -9, 7, 0, -3, -3, -5, -5, -10, 9, -6, 0, 1, -5, -5, -8, 2, 2, -4, 9, 4, 0, -7, -7, 5, -6, -6, 3, -4, -3, -5, -7, 7, -1, -4, -9, 3, 3, 1, -8, 3, 10, -6, 0, 8, -6, -1, 1, -2, 0, 1, -1, 9, -5, 8, -4, -8, -5, -3, -3, -3, 7, 3, 0, 1, 7, 1, -10, -5, -2, -9, -10, -3, -3, -3, 2, 3, -6, 5, -1, 7, -3, 7, -4, 9, -5, 10, 2, 3, 10, 5, 9, -5, -1, -1, 1, 1, -10, 3, -8, 9, 2, -4, -5, -4, 10, -4, -5, -1, -5, 3, -9, 0, 6, -8, 7, 10, 6, -3, -7, 6, -1, -1, 1, -1, -7, 10, 9, -1, -9, -3, -9, 7, 0, -3, -3, -5, -5, -10, 9, -6, 0, 1, -5, -5, -8, 2, 2, -4, 9, 4, 0, -7, -7, 5, -6, -6, 3, -4, -3, -5, -7, 7, -1, -4, -9, 3, 3, 1, -8, 3, 10, -6, 0, 8, -6, -1, 1, -2, 0, 1, -1, 9, -5, 8, -4, -8, -5, -3, -3, -3, 7, 3, 0, 1, 7, 1, -10, -5, -2, -9, -10, -3, -3, -3, 2, 3, -6, 5, -1, 7, -3, 7, -4, 9, -5, 10, 2, 3, 10, 5, 9, -5, -1, -1, 1, 1, -10, 3, -8, 9, 2, -4, -5, -4, 10, -4, -5, -1, -5, 3, -9, 0, 6, -8, 7, 10, 6] 
