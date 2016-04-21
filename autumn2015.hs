-- CS4620 Winter 2015 exam

-- Question 1
-- a)
-- "Lazy evaluation", also known as "call-by-need evaluation", means that an
-- expression is evaluated at most once, if/when it's needed, and only to the
-- extent that it is needed. It is one implementation of "non-strict
-- evaluation". Haskell uses lazy evaluation, whereas imperative languages
-- (such as C, Java, and Python) use "strict evaluation", whereby an
-- expression is evaluated as soon as it's bound to a variable.

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
-- and1 is eager in both its arguments, whereas and2 is lazy in its second
-- argument.

-- Question 2
-- a)
encode :: String -> [(Char, Int)]
encode = foldr step []
  where
    step x [] = [(x, 1)]
    step x ps@((x', n') : ps')
        | x == x' = (x', n' + 1) : ps'
        | otherwise = (x, 1) : ps

-- b)
decode :: [(Char, Int)] -> String
decode = concatMap $ \(x, n) -> replicate n x

-- Question 3
-- a)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ _      _      = []

-- b)
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- c)
-- mystery takes a value of type [a -> b] (a list of functions) and a value
-- of type [a] (a list) as arguments, and returns the list obtained by
-- applying each element of the first list to the corresponding element in
-- the second list.

-- d)
mystery :: [a -> b] -> [a] -> [b]
mystery = zipWith ($)
