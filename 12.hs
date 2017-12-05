import Data.List

tNums = scanl (+) 1 [2..]

divisors x = filter (\y -> x `mod` y == 0) $ [2..x]

primeFactorization n = pf n []
  where
    pf x xs
      | x == 1 = xs
      | otherwise = pf (x `div` a) (a:xs)
        where
        a = head $ divisors x

numDivisors = product . map ((+1) . length) . group . primeFactorization

main = print $ head . filter ((>500) . numDivisors) $ tNums
