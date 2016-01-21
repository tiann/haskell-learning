-- Insert an element at a given position into a list.
--
-- Example:
--
-- * (insert-at 'alfa '(a b c d) 2)
-- (A ALFA B C D)
-- Example in Haskell:
--
-- P21> insertAt 'X' "abcd" 2
-- "aXbcd"
--

insertAt :: a -> [a] -> Int -> [a]
insertAt e es index = insert where
  slice = splitAt (index - 1) es
  insert = fst slice ++ [e] ++ snd slice

insertAt' :: a -> [a] -> Int -> [a]
insertAt' e es n | n <= 1 = e: es
insertAt' e [] _ = [e]
insertAt' e (y: ys) n = y: insertAt' e ys (n-1)
