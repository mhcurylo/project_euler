import Data.List (permutations, maximum)

isPrime x = (==x) .  head . filter ((== 0) . mod x) $ [2..]

ninetotwo = [9,8..2]

pandigs = zipWith take ninetotwo (repeat "123456789")

main = print $ maximum . head . dropWhile (==[]) . map (filter isPrime) . map (map (read :: String -> Integer)) $ map permutations pandigs
