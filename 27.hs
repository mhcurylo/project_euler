import Data.List (maximumBy, sortBy)
import Control.Arrow
import Data.Ord

primes = remove [2..]
remove (x :xs) = x : remove [z | z <- xs, z `mod` x > 0]

isPrime x = (== x') . head $ dropWhile (< x') primes
  where 
  x' = abs x

as :: [Int]
as = [-999, -998..999]
bs :: [Int]
bs = [-1000, -999..1000]

ab = [(a, b) | a <- as, b <- bs, isPrime a, isPrime (a + b + 1)]

quadr (a, b) n = n*n + a*n + b

allQuadr t@(a, b) = takeWhile isPrime $ map (quadr t) [0..] 

tuppleProduct = uncurry (*)

main = print $ fst . maximumBy (comparing snd) $ map (tuppleProduct &&& length . allQuadr) ab 
