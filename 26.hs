import Data.List
import Control.Arrow
import Data.Ord

main = print $ fst .  maximumBy (comparing snd) . map (id &&& remicycl) $ [1..999]

remicycl :: Int -> Int
remicycl x = rc 10 []
  where
    rc 0 ys = 0
    rc y ys = if z `elem` ys
          then (+1) . length . takeWhile (/=z) $ ys 
          else rc (10*z) (z:ys)
      where 
       z = y `mod` x

