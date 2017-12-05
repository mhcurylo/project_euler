import qualified Data.Map as M
import Data.List
import qualified Data.Set as S

primes :: [Integer] -> [Integer] 
primes xs = sieve' xs M.empty 
  where 
  sieve' [] t = []
  sieve' (x:xs) t = 
    case M.lookup x t of
      Just f -> sieve' xs (foldl reinsert (M.delete x t) f)
      Nothing -> x : sieve' xs (M.insert (x*x) [x] t)
      where
      reinsert tbl p = M.insertWith (++) (x+p) [p] tbl


myPrimes = S.fromList . primes $ [2..1000000]

isPrime = flip S.member $ myPrimes

main = print $ length . filter isCircular . S.elems $ myPrimes

isCircular = all isPrime . circles  
  where 
  circles x = map (read :: String -> Integer) . takeWhile (/= sx) $ rotations sx  
    where
    sx = show x
    rotations (x:xs) = y : rotations y
      where
      y = (xs ++ [x])


