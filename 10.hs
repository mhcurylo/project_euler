import qualified Data.Map as M

sieve :: [Int] -> [Int]
sieve xs = sieve' xs M.empty
  where
    sieve' [] tab = [] 
    sieve' (x:xs) tab = case M.lookup x tab of 
                           Nothing -> x : sieve' xs (M.insert (x*x) [x] tab)
                           Just facts -> sieve' xs (foldl reinsert (M.delete x tab) facts)
                              where
                              reinsert t p = M.insertWith (++) (x + p) [p] t

main = print $ sum . takeWhile (< 2000000) $ sieve [2..]
