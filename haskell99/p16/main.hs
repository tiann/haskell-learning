-- (**) Drop every N'th element from a list.
--
-- Example:
--
-- * (drop '(a b c d e f g h i k) 3)
-- (A B D E G H K)
-- Example in Haskell:
--
-- *Main> dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a]
dropEvery xs num | num <= 0 = xs
dropEvery xs num = foldr delete_n [] (zip xs [1..])
  where
    delete_n x acc 
      | snd x `mod` num == 0 = acc
      | otherwise = fst x: acc

-- 元组的匹配需要使用 "，"
dropEvery' :: [a] -> Int -> [a]
dropEvery' xs num | num <= 0 = xs
dropEvery' xs num = foldr delete_n [] (zip xs [1..])
  where
    delete_n (x, index) acc 
      | index `mod` num == 0 = acc
      | otherwise = x: acc
