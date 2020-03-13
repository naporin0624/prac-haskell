main = do
  print "HelloWorld"
  let arr = [10, 2, 3, 1, 4, 5, 6, 7, -1]
  print(quicksort arr)
  print(maximum' arr)
  print(replicate' 10 1)
  print(replicate' 0 1)
  

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0  = []
  | otherwise = x : replicate' (n - 1) x