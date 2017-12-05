import Control.Arrow
import Data.Array
import Data.Tuple
import Data.List (maximumBy)

sqrs :: Int -> [(Int, Int)]
sqrs n = (n+n, n):map (first (+n)) (sqrs n)

sqrsTill :: Int -> [(Int, Int)]
sqrsTill n = concatMap (takeWhile ((< n) . fst) . sqrs) $ [1..n]

properDivisorsSum :: Int -> Array Int Int
properDivisorsSum n = accumArray (+) 0 (0,n -1 ) (sqrsTill n)

amicableNumbers :: Int -> [(Int, Int)]
amicableNumbers  n = filter (\(a, b) -> b < n && (pdSum ! b) == a && a > b) $ assocs pdSum
  where 
    pdSum = properDivisorsSum n

tsum :: (Int, Int) -> Int
tsum (a, b) = a + b

main = do
  print $ sum . map tsum $ amicableNumbers 10000
