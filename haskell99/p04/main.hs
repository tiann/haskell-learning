--(*) Find the number of elements of a list.
--
--Example in Haskell:
--
--Prelude> myLength [123, 456, 789]
--3
--Prelude> myLength "Hello, world!"
--13

myLength :: Num b => [a] -> b
myLength [] = 0
myLength (x: xs) = 1 + myLength xs

-- if there is not type declaring, compiler will complian
myLength' :: Num b => [a] -> b
myLength' = foldl (\acc _ -> acc + 1) 0

-- 这里！！ foldr接受一个函数，这函数接受两个参数回传一个值
-- 使用curring的话 我们可以接受一个参数，回传一个函数，这个函数接受一个参数，回传一个函数！！
myLength'' :: Num b => [a] -> b
myLength'' = foldr (\_ -> (+1)) 0
