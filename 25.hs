fib = 1:1:zipWith (+) fib (tail fib)

main = print $ (+1) . length $ takeWhile ((< 1000)  . length . show) fib 
