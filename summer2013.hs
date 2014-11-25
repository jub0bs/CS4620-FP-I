-- CS4620 Summer 2013 exam

-- Question 1
-- a)
-- count x xs : the number of times that item 'x' appears in list 'xs'
count :: Eq a => a -> [a] -> Int
count x = foldr (\y -> \acc -> (if y == x then 1 else 0) + acc) 0

-- b)
-- remdups xs : a copy of list 'xs' from which duplicate items have been
--              removed
remdups :: Eq a => [a] -> [a]
remdups xs = remdups' xs []
    where remdups' []     _ = []
          remdups' (x:xs) l
              | x `elem` l  = remdups' xs l
              | otherwise   = x : remdups' xs (x:l)

-- c)
-- occurences xs : the list of tuples of distinct items and their number of
--                 appearances in list 'xs'
occurences :: Eq a => [a] -> [(a,Int)]
occurences xs = [ (x, n) | x <- remdups xs, let n = count x xs ]

-- Question 2
-- a) and b)
-- iterate' f x : the list obtained by applying function 'f' to item 'x'
--               recursively: [x, f x, f (f x), f (f (f x)), ...]
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- c)
-- powers n : the infinite list of positive integer powers of number 'n'
powers :: Num a => a -> [a]
powers n = iterate (\powerSoFar -> powerSoFar * n) n

-- Question 3
-- a)
-- isPermutation f s : is function 'f' a permutation on list 's'?
isPermutation :: Eq a => (a -> a) -> [a] -> Bool
isPermutation f [] = True
isPermutation f s  = all (\x -> f x `elem` s) s && remdups l == l
    where l = map f s

-- b)
-- inverse f s : the inverse of the permutation 'f' on the list 's'
inverse :: Eq a => (a -> a) -> [a] -> (a -> a)
inverse f s = \y -> head [ x | x <- s, f x == y ]

-- TESTING
-- let {f 1 = 3 ; f 2 = 1 ; f 3 = 2}
-- isPermutation f [1 .. 3] returns True
-- let {g 1 = 3 ; g 2 = 2 ; g 3 = 2}
-- isPermutation g [1 .. 3] returns False

-- inverse f [1 .. 3] $ 1 returns 2
-- inverse f [1 .. 3] $ 2 returns 3
-- inverse f [1 .. 3] $ 3 returns 1
