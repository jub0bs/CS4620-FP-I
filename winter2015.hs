-- CS4620 Winter 2015 exam

-- Question 1
-- a)
-- "Lazy evaluation", also known as "call-by-need evaluation", means that an
-- expression is evaluated at most once, if/when it's needed, and only to the
-- extent that it is needed. It is one implementation of "non-strict
-- evaluation". Haskell uses lazy evaluation, whereas imperative languages
-- (such as C, Java, and Python) use "strict evaluation", whereby an
-- expression is evaluated as soon as it's bound to a variable.
--
-- b)
-- If function && were strict in its second parameter, then
--
--     all :: [Bool] -> Bool
--     all = foldr (&&) True
--
-- would not terminate on input lists that contain a False value.
-- If function || were strict in its second parameter, then
--
--     any :: [Bool] -> Bool
--     any = foldr (||) True
--
-- would not terminate on input lists that contain a True value.
-- If data constructor : were strict in its second field, then infinite
-- lists would not be possible; for instance, applying
--
--  repeat :: a -> [a]
--  repeat x = let xs = x : xs in xs
--
--  would result in a non-terminating computation instead of an infinite
--  list.

-- c)
-- [ (x, y, z) | x <- [1 .. ], y <- [1 .. ], z <- [1 .. ],
--               x < y, y < z, x * x + y * y == z * z ]
-- The list comprehension first examines all the candidates of the form
-- (1, 1, z) for z ranging from 1 to infinity;  therefore, it results in
-- a non-terminating computation. A valid (though inefficient) approach is
-- as follows:
pytrips :: [(Int, Int, Int)]
pytrips = [ (x, y, z)
          | z <- [3 .. ]
          , y <- [2 .. z - 1]
          , x <- [1 .. y - 1]
          , x * x + y * y == z * z
          ]

-- Question 2
maxItems :: [(a, Int)] -> ([a], Int)
maxItems = foldr step ([], -1)
  where
    step (x', n') (xs, n)
        | n' < n = (xs, n)
        | n' == n = (x' : xs, n)
        | otherwise = ([x'], n')

-- Question 3
-- a)
satCount :: [a -> Bool] -> [a] -> [(a, Int)]
satCount ps xs = zip xs (map f xs)
  where
    f x = length (filter ($ x) ps)

-- b)
-- If ps were infinite (and xs finite), the result would be a
-- non-terminating computation, because computing the second element
-- of the head of the "resulting list" would require checking how many of
-- an infinite number of predicates the head of the input list satisfies.

-- c)
-- If xs were infinite (and ps finite), the result would be an infinite
-- list.
