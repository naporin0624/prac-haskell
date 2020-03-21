import Data.List
import Data.Char
import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
isIn needle haystack = any (isPrefixOf needle) (tails haystack)

encode :: Int -> String -> String
encode offset = map (\c -> chr $ ord c + offset)

decode :: Int -> String -> String
decode offset = map(\c -> chr $ ord c - offset)

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find(\x -> digitSum x == 40) [1..]

firstTo :: Int -> Maybe Int
firstTo n = find(\x -> digitSum x == n) [1..]

findKey :: (Eq k) => k -> [(l, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

main = do
  print(numUniques [1,2,3,4,4,1,21,321,312,3,5])
  print(nub $ sort [1,2,3,4,6,5,4,4])
  let word = "hey these are the words in this sentence"
  print(words word)
  print(group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7])
  print(group $ sort ["boom", "bip", "bip", "boom", "boom"])
  print(wordNums word)
  print(tails "party")
  print(isPrefixOf "hawaii" "hawaii joe")
  print(any (isPrefixOf "art") (tails "party"))
  print(isIn "art" "party")
  print("art" `isIn` "party")
  print(map ord "abcdefgh") 
  print(encode 1 "hogehoge")
  print(decode 1 $ encode 1 "hogehoge")
  print(foldl (+) 0 (replicate 100 1))
  print(foldl (+) 0 (replicate 1000000 1))
  print(foldl' (+) 0 (replicate 1000000 1))
  print(digitToInt '2')
  print(find (>4) [3,4,5,6,7])
  print(find (=='z') "mojolnir")
  print firstTo40
  print(firstTo 27)
  print(firstTo 13)
  print(firstTo 40)
  let phoneBook = [
    ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]
