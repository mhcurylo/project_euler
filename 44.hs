import Control.Arrow ((&&&))

pentagonal :: [Integer]
pentagonal = map floor $ map (\n -> n*(3*n - 1)/2) [1..]

isPentagonal :: Integer -> Bool
isPentagonal n = uncurry (==) . (floor &&& ceiling) $ ((sqrt (24 * (fromIntegral n) + 1)  + 1) / 6)

pentDiffs = [(a - b) | a <- pentagonal, 
                      b <- reverse $ takeWhile (<= a) pentagonal,
                      isPentagonal (a + b),
                      isPentagonal (a - b)]

di = uncurry (-)
su = uncurry (+)

main = print $ head $ pentDiffs 
