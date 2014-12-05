-- CS4620 Summer 2014 exam

-- Question 1
-- a)
-- take' n xs : the list formed by taking the first 'n' items of list 'xs'
take' :: Int -> [a] -> [a]
take' _ []       = []
take' n (x:xs)
    | n <= 0     = []
    | otherwise  = x : take' (n - 1) xs

-- b)
-- drop' n xs : the list formed by dropping the first 'n' items of list 'xs'
drop' :: Int -> [a] -> [a]
drop' _ []       = []
drop' n (x:xs)
    | n <= 0     = x:xs
    | otherwise  = drop' (n - 1) xs

-- c)
-- takewhile' p xs : the longest prefix of list 'xs' whose components all
--                   satisfy predicate 'p'
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []     = []
takeWhile' p (x:xs)
    | p x           = x : takeWhile' p xs
    | otherwise     = []

-- d)
-- dropWhile' p xs : the longest suffix of list 'xs' whose first component
--                   does not satisfy predicate 'p'
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []     = []
dropWhile' p (x:xs)
    | p x           = dropWhile' p xs
    | otherwise     = x : xs

-- e)
-- zipWith' f xs ys : the list formed by combining lists 'xs' and 'ys' using
--                    function 'f'; the length of the resulting list is the
--                    smaller of the lengths of lists 'xs' and 'ys'.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _      []     = []
zipWith' _ []     _      = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- Question 2
-- isStairs xs : is the finite list 'xs' a stairs?
isStairs :: (Eq a, Num a) => [a] -> Bool
isStairs []         = False
isStairs [x]        = False
isStairs (n1:n2:ns) = n1 /= n2 && isStairs' (n2:ns) (n2 - n1)
    where isStairs' []         _   = True
          isStairs' [x]        _   = True
          isStairs' (x1:x2:xs) gap = x2 - x1 == gap && isStairs' (x2:xs) gap

-- Question 3
-- a)
-- iterate' f x : the list obtained by applying function 'f' to item 'x'
--                recursively: [x, f x, f (f x), f (f (f x)), ...]
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- b)
-- reps : the infinite list that has, as its nth item, a list composed of n
--        copies of integer n
reps :: Integral a => [[a]]
reps = iterate (\l@(h:_) -> map (+1) (h:l)) [1]

-- c)
-- pascal : the infinite list that has, as its nth item, the nth row of
-- Pascal's triangle
pascal :: Integral a => [[a]]
pascal = iterate (\l@(_:xs) -> 1 : zipWith (+) l xs ++ [1]) [1]
