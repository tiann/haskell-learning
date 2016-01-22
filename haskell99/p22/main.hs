-- Create a list containing all integers within a given range.
--
-- Example:
--
-- * (range 4 9)
-- (4 5 6 7 8 9)
-- Example in Haskell:
--
-- Prelude> range 4 9
-- [4,5,6,7,8,9]

range :: Int -> Int -> [Int]
range min max = [min..max]

range' :: Int -> Int -> [Int]
range' min max
  | min == max = [min]
  | min < max = min: range' (min + 1) max
  | min > max = min : range' (min - 1) max
