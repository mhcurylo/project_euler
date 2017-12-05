


main = readFile "13.data" >>= print . take 10 . show . sum . map read . lines
