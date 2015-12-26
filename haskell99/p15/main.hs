-- (**) Replicate the elements of a list a given number of times.
--
-- Example:
--
-- * (repli '(a b c) 3)
-- (A A A B B B C C C)
-- Example in Haskell:
--
-- > repli "abc" 3
-- "aaabbbccc"

repli :: [a] -> Int -> [a]
repli _ num | num <= 0 = []
repli xs 1 = xs
repli [] _ = []
repli (x:xs) num = replicate num x ++ repli xs num 

repli' xs num = xs >>= replicate num
