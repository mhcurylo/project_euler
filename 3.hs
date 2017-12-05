q = 600851475143 

divisors x = filter (\y -> x `mod` y == 0) $ [2..x] 

primeFactorization n = pf n []
  where
  pf x xs 
    | x == 1 = xs
    | otherwise = pf (x `div` a) (a:xs)
    where
      a = head $ divisors x  

largestFac = head . primeFactorization

main = print $ largestFac q
