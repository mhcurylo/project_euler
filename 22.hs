import Control.Arrow
import Data.List
import qualified Data.Map.Strict as M
import Data.Char

main = readFile "22.data" >>= countResult >>= print


countResult = return . countPoints . extractNames


countPoints = sum . map mulT . zip [1,2..] . M.elems . M.fromList . map (id &&& value)
  where
    base = ord 'A' - 1
    value = sum . map (\c -> ord c - base) 
    mulT (a, b) = a * b

extractNames = splitOn (==',') . filter (/='\"')


splitOn :: Eq a => (a -> Bool) -> [a] -> [[a]] 
splitOn f xs = case dropWhile f xs of
                 []  -> []
                 ys -> ys': splitOn f xs'
                       where (ys', xs') = break f ys
                 

