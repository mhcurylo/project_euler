import Data.Char

main = print $ sum . map digitToInt . show  $ fact !! 100


fact =  1:zipWith (*) [1..] fact
