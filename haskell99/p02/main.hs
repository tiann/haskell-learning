--(*) Find the last but one element of a list.
--
--(Note that the Lisp transcription of this problem is incorrect.)
--
--Example in Haskell:
--
--Prelude> myButLast [1,2,3,4]
--3
--Prelude> myButLast ['a'..'z']
--'y'

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [x] = error "single list"
myButLast [x, _] = x
myButLast (_: xs) = myButLast xs

myButLast' = last . init
