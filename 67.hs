import Data.List (maximum, foldl1')

max2 [] = []
max2 xs = maximum (take 2 xs) : max2 (tail xs)

max2head xs = head xs : max2 xs

main = do 
 f <- readData "67.data" 
 print $ countResult f

readData = fmap (map (map read . words) . lines) . readFile
countResult = maximum . foldl1' (\a b -> zipWith (+) (max2head a) b)
