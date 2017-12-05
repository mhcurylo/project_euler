import qualified Data.Map.Strict as M
import Control.Arrow ((&&&))

primes :: [Integer] -> [Integer]
primes xs = sieve xs M.empty

sieve :: [Integer] -> M.Map Integer [Integer] -> [Integer]
sieve [] m = [] 
sieve (x:xs) m = case M.lookup x m of
  Just y -> sieve xs (foldl reinsert (M.delete x m) y)
  Nothing -> x : sieve xs (M.insert (x*x) [x] m) 
  where
  reinsert t v = M.insertWith (++) (x+v) [v] t

allPrimes = primes [2..]

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime x = (== x) . head . filter ((==0) . mod x) $ allPrimes 

trunc [] = []
trunc xs = xs : trunc (tail xs)

isTruncatable :: Integer -> Bool
isTruncatable = all (isPrime . read) . uncurry (++) . (trunc &&& map reverse . trunc . reverse) . show

main = print $ zip [1..] . take 11 . filter isTruncatable $ drop 8 allPrimes
--filter isTruncatable $ drop 8 allPrimes

