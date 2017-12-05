import Data.List
import Data.Array
import Control.Arrow
import qualified Data.Set as S

maxAN = 28123

sqrs n = (n + n, n) : map (first (+ n)) (sqrs n)
sqrsTill n = concatMap (takeWhile ((<n) . fst) . sqrs) $ [2..(n `div` 2)]

pdSums n = accumArray (+) 1 (2, n-1) (sqrsTill n)

abundantNumbers = map fst . filter (\(a,b) -> b > a) . assocs . pdSums 

cartProd xs ys = [(x, y) | x <- xs, y <- ys]
cartProd1 xs = cartProd xs xs

sumsOfAbundantNumbers = map (uncurry (+)) . filter (uncurry (>=)) . cartProd1 . abundantNumbers

main = print $ sum $ (S.fromList [1..maxAN]) S.\\ (S.fromList $ sumsOfAbundantNumbers maxAN)
