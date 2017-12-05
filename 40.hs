import Data.Char (digitToInt)

chc = concatMap show $ [1..]

pos = take 7 . map (+ (-1)) $ iterate (*10) 1

main = print $ product . map digitToInt $ zipWith (!!) (repeat chc) pos


