import Data.List (permutations, nub)

main = print $ sum . nub . map third . filter isProd . concatMap intoNums $ permutations ['1', '2', '3', '4', '5', '6', '7', '8', '9']

third (a, b, c) = c
isProd (a, b, c) = a * b == c

intoNums xs = [(a, b, c), (d, e, f)]
   where
     a = toNum 3 xs
     b = toNum 2 (drop 3 xs)
     c = toNum 4 (drop 5 xs)
     d = toNum 1 xs
     e = toNum 4 (drop 1 xs)
     f = toNum 4 (drop 5 xs)

toNum :: Int -> String -> Int
toNum x = read . (take x)
