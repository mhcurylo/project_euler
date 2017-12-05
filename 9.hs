main = print $ product . head $  [[a,b,c] | c <- [3..997], b <- [2..c], a <- [1..b], a*a + b*b == c*c, a + b + c == 1000]
