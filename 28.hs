

diagonals = True : concatMap (\x -> concat $ replicate 4 (replicate x False ++ [True])) [1,3..]

maxNum = 1001*1001
main = print $ sum . map fst . filter snd $ zip [1,2..maxNum] diagonals
