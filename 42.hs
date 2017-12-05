import Data.Map (fromList, toList, (!))
import Control.Arrow ((&&&))

triangular n = (n*(n+1))/2  
trinums = map triangular [1..]
alphanum = fromList $ zip ['A'..'Z'] [1..]

anum = (alphanum !)
isTri x = (==x) . head  $ dropWhile (<x) trinums

main = do
  fl <- readFile "./42.data"  
  print $ length . filter isTri . map (sum . map anum) . splitOn ',' . removeChar '"' $ fl

removeChar :: Char -> String -> String
removeChar a xs = case dropWhile (==a) xs of
  "" -> []
  xs' -> w ++ (removeChar a ws)
    where 
    (w, ws) = break (==a) xs'

splitOn :: Char -> String -> [String]
splitOn a xs = case dropWhile (== a) xs of 
  "" -> []
  xs' -> w : (splitOn a ws)
    where 
    (w, ws) = break (==a) xs'

