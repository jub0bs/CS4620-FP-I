-- CS4620 Autumn 2013 exam

-- Question 1
-- a)
-- powers n : the list of all positive powers of the number 'n'
powers :: Num a => a -> [a]
powers n = iterate (\powerSoFar -> powerSoFar * n) n

-- b)
-- factorials : the list of factorials of all positive integers
factorials :: [Integer]
factorials = 1 : zipWith (*) factorials [2 .. ]

-- Question 2
-- atLeast :: Int -> [a] -> Bool
-- atLeast n xs = length xs >= n
-- a) two problems:
--      1. inefficiency: computes the length of the list for no good reason
--      2. incompatible with infinite lists: will just hang if fed an infinite
--         list (because it will never end computing its length)

-- b)
-- atLeast n xs : are there at least 'n' items in the list 'xs'?
atLeast :: Int -> [a] -> Bool
atLeast n l
    | n <= 0     = True
    | otherwise  = case l of []     -> False
                             (x:xs) -> atLeast (n - 1) xs

-- Question 3
-- a)
-- from n : the increasing list of all integers from 'n' onwards
from :: Integer -> [Integer]
from n = [n .. ]

-- b)
-- unite xs ys : the list formed by alternately selecting items from infinite
--               lists 'xs' and 'ys'
unite :: [a] -> [a] -> [a]
unite (x:xs) (y:ys) = x : y : unite xs ys

-- c)
-- integers : the list of all integers, ordered as shown [0, 1, -1, 2, -2, ...]
integers :: [Integer]
integers = 0 : foldr (\n -> \acc -> n : -n : acc) [] [1 .. ]
