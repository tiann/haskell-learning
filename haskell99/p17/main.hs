-- (*) Split a list into two parts; the length of the first part is given.
--
-- Do not use any predefined predicates.
--
-- Example:
--
-- * (split '(a b c d e f g h i k) 3)
-- ( (A B C) (D E F G H I K))
-- Example in Haskell:
--
-- *Main> split "abcdefghik" 3
-- ("abc", "defghik")

split :: [a] -> Int -> ([a], [a])
split xs num = (take num xs, drop num xs)

split' :: [a] -> Int -> ([a], [a])
split' xs num = foldr splitHelper ([], []) (zip xs [1..])
  where
    splitHelper (x, index) (l1, l2)
      | index <= num = (x: l1, l2)
      | otherwise = (l1, x: l2)

split'' ::[a] -> Int -> ([a], [a])
split'' = flip splitAt
