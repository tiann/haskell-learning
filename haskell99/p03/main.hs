--(*) Find the K'th element of a list. The first element in the list is number 1.
--
--Example:
--
-- * (element-at '(a b c d e) 3)
--c
--Example in Haskell:
--
--Prelude> elementAt [1,2,3] 2
--2
--Prelude> elementAt "haskell" 5
--'e'

elementAt :: (Num b, Ord b)  => [a] -> b -> a
elementAt [] _ = error "empty list"
elementAt (x:xs) index
    | index <= 0 = error "invalid index"
    | index == 1 = x
    | otherwise = elementAt xs $ index - 1
