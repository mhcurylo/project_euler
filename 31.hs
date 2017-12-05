-- 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p
coins = [1,2,5,10,20,50,100,200]
coinz = reverse coins

changes 0 _ = [[]]
changes x [] = []
changes x yy@(y:ys) = if y <= x 
    then map (y:) (changes (x - y) yy) ++ changes x ys
    else changes x ys 

main = print $ length $ changes 200 coinz 

 
