import qualified Data.Map as M
import qualified Data.List as L
import Data.Ord (comparing)
import Control.Arrow ((&&&))

alltris :: [(Integer, Integer, Integer)]
alltris = [(a,b,c) | a <- [1..998], b <- [1..(a-1)], 
                     let c' = sqrt (fromInteger ( a*a - b*b )), 
                     let c = floor c', 
                     c == ceiling c', 
                     a > b, c <= b, a + b + c <= 1000]  

sumTri (a,b,c) = a + b + c

main = print $ fst . L.maximumBy (comparing snd) $ M.toList . fmap length . M.fromListWith (++) . map (sumTri &&& (:[])) $ alltris

