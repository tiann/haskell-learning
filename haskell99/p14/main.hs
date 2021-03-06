--(*) Duplicate the elements of a list.
--
--Example:
--
-- * (dupli '(a b c c d))
--(A A B B C C C C D D)
--Example in Haskell:
--
-- > dupli [1, 2, 3]
--[1,1,2,2,3,3]

dupli :: [a] -> [a]
dupli = foldr copy []
  where
    copy x acc = x: x: acc

dupli' xs = xs >>= (\x -> [x, x])
