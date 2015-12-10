--(*) Reverse a list.
--
--Example in Haskell:
--
--Prelude> myReverse "A man, a plan, a canal, panama!"
--"!amanap ,lanac a ,nalp a ,nam A"
--Prelude> myReverse [1,2,3,4]
--[4,3,2,1]
--

-- 这个实现性能不行，如果列表很大，简直是个灾难
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x: xs) = (myReverse xs) ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldr (\x acc -> acc ++ [x]) []

myReverse'' :: [a] -> [a]
myReverse'' = foldl (\acc x -> x: acc) []

-- 观察上面的函数，发现实际上是(:)算子把参数反转了，所以有下面的实现
myReverse''' :: [a] -> [a]
myReverse''' = foldl (flip (:)) []
