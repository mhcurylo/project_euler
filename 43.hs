import Data.List (permutations)

divis = [2,3,5,7,11,13,17]

pandigitals = permutations "1234567890"

allThrees xs = map dropTake [1..7]
  where
  dropTake x = take 3 . drop x $ xs

isDiv :: Integer -> String -> Bool
isDiv x xs = (== 0) $ mod (read xs) x

isSubDiv = all (== True) . zipWith isDiv divis . allThrees 

main = print $ sum . map read $ filter isSubDiv pandigitals








