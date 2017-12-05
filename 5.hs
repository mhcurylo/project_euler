import Data.List
import qualified Data.Map as M

divisors x = filter (\y -> x `mod` y == 0) $ [2..x] 

primeFactorization n = pf n []
  where
  pf x xs 
    | x == 1 = xs
    | otherwise = pf (x `div` a) (a:xs)
    where
      a = head $ divisors x  

main = print $ foldl (*) 1 . map (uncurry (^)) . M.assocs . M.unionsWith max $ map (M.fromListWith (+)  . (`zip` repeat 1) . primeFactorization) [2..20] 
