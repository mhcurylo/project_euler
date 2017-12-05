
triangle :: [Integer]
triangle = map floor $ map (\n -> (n * (n + 1))/2) [1..]
pentagonal :: [Integer]
pentagonal = map floor $ map (\n -> (n * (3*n - 1))/2) [1..]
hexagonal :: [Integer]
hexagonal = map floor $ map (\n -> n * (2*n - 1)) [1..]


tripenhex :: [Integer]
tripenhex = next $ tripenhex' triangle pentagonal hexagonal
 where
 tripenhex' xx@(x:xs) yy@(y:ys) zz@(z:zs)
  | x == y && y == z = (x, (xs, ys, zs)) 
  | x > y = tripenhex' xx ys zz
  | y > z = tripenhex' xx yy zs
  | otherwise = tripenhex' xs yy zz
 next (a, (b, c, d)) = a : next (tripenhex' b c d)


main = print $ tripenhex !! 2

