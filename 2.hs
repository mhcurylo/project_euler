fi = 1 : 1 : zipWith (+) fi (tail fi)

main = print $ sum . filter (\y -> mod y 2 == 0) . takeWhile (< 4000000) $ fi
