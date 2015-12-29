-- (**) Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
--
-- Examples:
--
-- * (rotate '(a b c d e f g h) 3)
-- (D E F G H A B C)
--
-- * (rotate '(a b c d e f g h) -2)
-- (G H A B C D E F)
-- Examples in Haskell:
--
-- *Main> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
--  
--  *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
--  "ghabcdef"

rotate :: [a] -> Int -> [a]
rotate xs n
  | n >= 0 = snd slice ++ fst slice
  | otherwise = rotate xs (length xs + n)
  where
    slice = splitAt n xs

