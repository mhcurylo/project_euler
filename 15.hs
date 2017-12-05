list = take 20 $ repeat 1 
matrix = take 20 . iterate (scanl1 (+)) $ list 

main = print $ last . last $ matrix 

