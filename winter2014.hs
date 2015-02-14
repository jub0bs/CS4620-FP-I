-- Question 1
-- a)
-- [ ⟨expr⟩ | ⟨var⟩ <- ⟨list⟩, ⟨condition⟩ ]
-- ... is equivalent to ...
-- map (\ ⟨var⟩ -> ⟨expr⟩) $ filter (\ ⟨var⟩ -> ⟨condition⟩) ⟨list⟩

-- b)
-- Static type checking (as used in C and Haskell) means that types are
-- determined at compile time rather that at run time.
-- Conversely, dynamic type checking (as used in Python and Lua) means
-- that types are determined at run time rather than at compile time.

-- Static vs. dynamic typing
-- Static:                     | Dynamic:
--   * greater robustness      |   * fast prototyping
--   * smaller executables     |   * ease of implementation
--  (* execution speed)        |
--
-- c)
-- Three advantages of explicitly specifying a Haskell expressions's type:
--   1. Form of documentation.
--   2. Greater control over types; for instance, Haskell, by default,
--      infers '1' to be of type Integer, but you may want to use Int,
--      instead, for better performance.
--   3. Abstraction.

-- Question 2
partialSums :: Num a => [a] -> [a]
partialSums ns = zipWith (+) ns (0 : partialSums ns)

-- Question 3
-- a)
from :: Int -> [Int]
from = iterate (+ 1)

-- b)
powerLists :: [[Int]]
powerLists = map powersOf $ from 1
    where powersOf n = iterate (* n) 1

-- c)
powerLists' :: [[Int]]
powerLists' = [ [ n ^ p | p <- [ 0 .. ] ] | n <- [ 1 .. ] ]

-- d)
facTuples :: [(Int,Int)]
facTuples = iterate ( \(n,factn) -> let n' = n + 1
                                    in (n', n' * factn) ) (1,1)
