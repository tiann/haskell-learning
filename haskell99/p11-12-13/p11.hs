--(*) Modified run-length encoding.
--
--Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
--
--Example:
--
-- * (encode-modified '(a a a a b c c a a d e e e e))
--((4 A) B (2 C) (2 A) D (4 E))
--Example in Haskell:
--
--P11> encodeModified "aaaabccaadeeee"
--[Multiple 4 'a',Single 'b',Multiple 2 'c',
-- Multiple 2 'a',Single 'd',Multiple 4 'e']
--
--
import Data.List (group)
data Group a = Multiple Int a | Single a
  deriving Show

splitGroup :: [a] -> Group a
splitGroup [x] = Single x
splitGroup all@(x:xs) = Multiple (length all) x

encodeModified :: Eq a => [a] -> [Group a]
encodeModified xs = map splitGroup (group xs)
 
--(**) Decode a run-length encoded list.
--
--Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
--
--Example in Haskell:
--
--P12> decodeModified 
--       [Multiple 4 'a',Single 'b',Multiple 2 'c',
--               Multiple 2 'a',Single 'd',Multiple 4 'e']
--               "aaaabccaadeeee"

decodeModified :: [Group a] -> [a]
decodeModified = concat . map decodeGroup where
  decodeGroup (Single x) = [x]
  decodeGroup (Multiple num x) = replicate num x

--(**) Run-length encoding of a list (direct solution).
--
--Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
--
--Example:
--
-- * (encode-direct '(a a a a b c c a a d e e e e))
--((4 A) B (2 C) (2 A) D (4 E))
--

encodeDirect :: Eq a => [a] -> [Group a]
encodeDirect = foldr convert [] where
  convert x [] = [Single x]
  convert x all@((Single y):xs) 
    | x == y = (Multiple 2 x): xs
    | otherwise = (Single x): all
  convert x all@((Multiple num y): xs)
    | x == y = (Multiple (num + 1) x): xs
    | otherwise = (Single x): all
