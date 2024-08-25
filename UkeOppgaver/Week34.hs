module Week34 where

-- check if list is empty

isEmpty :: [Int] -> Bool
isEmpty [] = True
isEmpty _ = False

safeHead :: [Int] -> [Int]
safeHead [] = []
safeHead (x : xs) = [x]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let lower = quickSort [y | y <- xs, y <= x]
      higher = quickSort [y | y <- xs, y > x]
   in lower ++ [x] ++ higher
