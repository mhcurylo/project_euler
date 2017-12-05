import  Data.List

sortedPermutations :: Ord a => [a] -> [[a]]
sortedPermutations = sort . permutations


main = print $ (sortedPermutations "0123456789") !! 999999
