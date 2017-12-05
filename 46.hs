import Data.Numbers.Primes (primes, isPrime)

tasq = map (\n -> 2*(n^2)) [1..]

oddcomp = filter (not . isPrime) . filter ((== 1) . (`mod` 2)) $ [3..]

sqrAndPrime x = sap primes tasq
  where
  sap pp@(p:ps) (t:ts)
   | x == p + t = True
   | p + (tasq !! 0) > x = False
   | p + t > x = sap ps tasq
   | otherwise = sap pp ts

main = print $ head $ filter (not . sqrAndPrime) oddcomp
