candidates :: [(Int,Int)]
candidates = [(a, b) | a <- [10..99], b <- [10..99], a < b]

main = print $ lowestDenominator . foldl (\a -> \b -> (fst a * fst b, snd a * snd b)) (1,1) $ filter isWerid candidates

lowestDenominator :: (Int, Int) -> Int
lowestDenominator (a, b) = b `div` (gcd a b)

isWerid (a, b) = y /= 0 && y' /= 0 && ((x == y' && (ab == y/x')) || (x' == y && (ab == x/y') ))
  where
  (x, y) = rat a
  (x', y') = rat b
  ab = toRational a / toRational b

rat :: Int -> (Rational, Rational) 
rat = mapTuple toRational . tupR . show

tupR :: String -> (Int, Int)
tupR (a:b:[]) = (read [a], read [b])
tupR xs = undefined

mapTuple f (a1, a2) = (f a1, f a2)
