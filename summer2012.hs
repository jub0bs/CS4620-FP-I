-- CS4620 Summer 2012 exam

-- Question 1
-- a)
-- allEqual xs : is every item in list 'xs' equal to every other such item?
allEqual :: Eq a => [a] -> Bool
allEqual []         = True
allEqual [x]        = True
allEqual (x1:x2:xs) = x1 == x2 && allEqual (x2:xs)

-- b)
-- allDifferent xs : is every item in the list 'xs' different from every
--                   other such item?
allDifferent :: Eq a => [a] -> Bool
allDifferent []  = True
allDifferent [x] = True
allDifferent l   = allDifferent' l []
    where allDifferent' []     _  = True
          allDifferent' (x:xs) l' = not (x `elem` l')
                                    && allDifferent' xs (x:l')

-- c)
-- countMax xs : the number of times that its maximum item occurs in the
--               non-empty list 'xs'
countMax :: Ord a => [a] -> Int
countMax [x]    = 1
countMax (x:xs) = countMax' xs x 1
    where countMax' []     z n = n
          countMax' (y:ys) z n
              | y > z          = countMax' ys y 1
              | y == z         = countMax' ys z (n + 1)
              | otherwise      = countMax' ys z n

-- Question 2
-- a) id :: a -> a

-- b) (.) :: (b -> c) -> (a -> b) -> (a -> c)

-- c) compose :: [a -> a] -> [a] -> a

-- d)
-- compose fs : the composition of the functions in list 'fs'
--              (recursive implementation)
compose :: [a -> a] -> a -> a
compose []     = id
compose (f:fs) = \x -> f $ compose fs x

-- e)
-- compose' fs : the composition of the functions in list 'fs'
--               (non-recursive implementation)
compose' :: [a -> a] -> a -> a
compose' = foldr (.) id

-- Question 3
-- a)
-- zipWith' f xs ys : the list formed by combining lists 'xs' and 'ys' using
--                    function 'f'; the length of the resulting list is the
--                    smaller of the lengths of lists 'xs' and 'ys'.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _      []     = []
zipWith' _ []     _      = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- b)
mystery = 1 : zipWith (*) mystery [1 .. ]
-- take 5 mystery returns [1,2,6,24,120]
-- a better name for 'mystery' would be 'factorials'
-- factorials :: (Num a, Enum a) => [a]

-- c)
-- [ (x, y, z) | x <- [1 .. ], y <- [1 .. ], z <- [1 .. ],
--               x < y, y < z, x * x + y * y == z * z ]
-- The list comprehension first examines all the candidates of the form (1,1,z)
-- for z ranging from 1 to infinity;  therefore, it never finds any Pythagorean
-- Triple and just hangs.
-- A valid (though inefficient) approach is as follows:
pytrips :: [(Integer,Integer,Integer)]
pytrips = [ (x, y, z) | z <- [3 .. ], y <- [2 .. z - 1], x <- [1 .. y - 1],
                        x * x + y * y == z * z ]
