{-# LANGUAGE FlexibleContexts #-}
main = do
  print "HoC"
  let multiTwoWithNine = multThree 9
  print(multiTwoWithNine 2 3)
  print(applyTwice (+3) 10)
  print(zipWith' (+) [4,2,5,6] [2,6,2])
  print(zip [1,2,3,4,5] "hello")
  print(flip' zip [1,2,3,4,5] "hello")
  print(zipWith (flip' div) [2, 2..] [10, 8, 6, 4, 2])
  print(map (+3) [1,5,3,1,6])
  print(quicksort [9,8,7,6,8,7,4,2,1,36,103,10])
  print largestDivisible
  print (takeWhile (/= ' ') "elephants know how to party")
  print (sum (takeWhile ( < 10000 ) (filter odd (map (^2) [1..]))))
  print (sum (takeWhile ( < 10000 ) [m | m <- [n^2 | n <- [1..]], odd m]))
  print (chain 100)
  print numLongChains
  let listOfFuns = map (*) [0..]
  print((listOfFuns !! 4) 5)
  print(reverse' [1,2,3,4,5,6])
  print(product' [1,2,3,4,4,5])
  print(filter' even [0..100])
  print(and' ([False] ++ (repeat True)))
  print(scanl (+) 0 [1,2,3,4,5,6,7,9,10])
  print(scanr (+) 0 [1,2,3,4,5,6,7,9,10])
  print(scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,10,7,8,2,1])
  print(scanl (flip (:)) [] [3,2,1])
  print sqrtSums
  print(sum (map sqrt [1..131]))
  print(sum (map sqrt [1..130]))
  print(sqrt 3 + 4 + 9)
  print(sqrt $ 3 + 4 + 9)
  print(sum (filter (>10) (map (*2) [2..10])))
  print(sum $ filter (>10) (map (*2) [2..10]))
  print(sum $ filter (>10) $ map (*2) [2..10])
  print(map ($ 3) [(4+), (10*), (^2), sqrt])
  print(map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24])
  print(map (negate . abs) [5,-3,-6,7,-3,2,-19,24])
  print(map (\xs -> negate (sum (tail xs))) [[1..5], [3..6], [1..7]])
  print(map (negate . sum . tail) [[1..5], [3..6], [1..7]])
  print(sum (replicate 5 (max 6.7 8.9)))
  print((sum . replicate 5) (max 6.7 8.9))
  print(sum .replicate 5 $ max 6.7 8.9)
  print(replicate 2 $ product $ map (*3) $ zipWith max [1, 2] [4, 5])
  print(replicate 2 . product . map (*3) $ zipWith max [1, 2] [4, 5])
  print(fn1 5)


multThree :: Int -> Int -> Int -> Int
multThree x y z = z * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f ( f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smaillerOrEqual = filter (<= x) xs
      larger = filter (> x) xs
  in quicksort smaillerOrEqual ++ [x] ++ quicksort larger

largestDivisible :: Integer
largestDivisible = head (filter p [100000, 99999..])
  where p x = mod x 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (div n 2)
  | odd n = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

reverse' :: (Num a) => [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc ) []

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

fn x = ceiling (negate (tan (cos (max 50 x))))
fn1 = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<1000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer
oddSquareSum' = sum $ takeWhile (<1000) $ filter odd $ map (^2) [1..]

oddSquareSum'' :: Integer
oddSquareSum'' = sum . takeWhile (<1000) . filter odd $ map (^2) [1..]