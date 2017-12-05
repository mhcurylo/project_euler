import Control.Arrow

main = print $ uncurry (-) $ ((^2) . sum) &&& (sum . map (^2)) $ [1..100] 
