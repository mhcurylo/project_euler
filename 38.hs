import Data.List

oneToNine = zipWith take [2..9] (repeat [1..9])

containsAllDigits z = foldl (&&) True $ (flip elem) z <$> "123456789" 

multis  = map (map (concatMap show)) $ zipWith (\x -> map (map (*x))) [1..] (repeat oneToNine)

limitMultis = concat . takeWhile (/= []) . map (takeWhile ((< 10). length)) $ multis

main = print $ maximum . map (read :: String -> Integer) . filter containsAllDigits . filter ((== 9) . length) $ limitMultis 
