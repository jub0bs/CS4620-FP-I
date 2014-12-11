-- CS4620 Autumn 2012 exam

-- Question 1
-- a)
-- drop' n xs : the list formed by dropping the first 'n' items in list 'xs'
drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' n (x:xs)
    | n <= 0    = (x:xs)
    | otherwise = drop' (n - 1) xs

-- b)
-- dropWhile' p xs : the longest suffix of list 'xs' such that the head does not
--                   satisfy predicate 'p'
dropWhile' _ []     = []
dropWhile' p (x:xs)
    | p x       = dropWhile' p xs
    | otherwise = x:xs

-- c)
-- fibonacci : the infinite list of Fibonacci Numbers [0,1,1,2,3,5,8,...],
--             where the first two items in this list are 0 and 1, and then
--             each subsecquent item is the sum of the preceding two
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- Question 2
-- a) and b)
-- iterate' f x : the list obtained by applying function 'f' to item 'x'
--                recursively: [x, f x, f (f x), f (f (f x)), ...]
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- c)
-- powers n : the infinite list of positive integer powers of number 'n'
powers :: Num a => a -> [a]
powers n = iterate (\powerSoFar -> powerSoFar * n) n

-- Question 3
-- a)
-- zipWith' f xs ys : the list formed by combining lists 'xs' and 'ys' using
--                    function 'f'; the length of the resulting list is the
--                    smaller of the lengths of lists 'xs' and 'ys'.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _      _      = []

-- b)
-- mystery ns = zipWith (+) ns ( 0 : mystery ns )
-- 'mystery [4, 1, 3, 5, 2]' returns '[4, 5, 8, 13, 15]'
-- a more appropriate name for 'mystery' would be 'runningSum'
-- runningSum :: Num a => [a] -> [a]

-- c)
-- runningSum ns : the list obtained by computing the running sum of numeric
--                 list 'ns'
runningSum :: Num a => [a] -> [a]
runningSum = runningSum' 0
    where runningSum' acc []     = []
          runningSum' acc (x:xs) = newAcc : runningSum' newAcc xs
              where newAcc = acc + x
