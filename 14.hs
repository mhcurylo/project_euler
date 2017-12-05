import Control.Arrow
import Data.Ord
import Data.List

colseq x
  | x == 1 = [1]
  | x `mod` 2 == 0 = x : (colseq $ x `div` 2)
  | otherwise = x : (colseq $ (3*x) + 1)

main = print $ fst . maximumBy (comparing snd) $ map (head &&& length) $ map colseq [1,2..1000000]
