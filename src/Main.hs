main = 
  print(quicksort [10, 1, 2, 3, 4, 5, 6, 7])

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerOrEqual = [a | a <- xs, a > x]
      larger = [a | a<- xs, a <= x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger
