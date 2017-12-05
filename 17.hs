

singles = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["", "ten", "twenty", "thirty", "fourty", "fifty", "sixty", "seventy", "eighty", "ninety"] 
thousands = "thousand"
hundred = "hundred"

dnum1s = (singles !!) . (`mod` 10)
dnumTeens = (teens !!) . (`mod` 10)
dnumTens = (tens !!) .(`mod` 10) . (`div` 10) 
dnumTens1s n
  | n `mod` 10 == 0 = dnumTens n
  | otherwise = dnumTens n ++ "-" ++ dnum1s n
dnumHuns n
  | n `div` 100 == 0 = h
  | otherwise = h ++ " and " ++ dnum (n `mod` 100)
   where
     h = dnum1s (n `div` 100) ++ " " ++ hundred

dnum :: Int -> String
dnum n 
  | n == 0 = "zero"
  | n < 10 = dnum1s n 
  | n < 20 = dnumTeens n
  | n < 100 = dnumTens1s n 
  | n < 1000 = dnumHuns n
  | n == 1000 = "one thousand"

dnumL = length . dnum  

main = print $ sum . map dnumL $ [1..1000]
