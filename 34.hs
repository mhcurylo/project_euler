import Control.Arrow
import Data.Char

facts = 1 : zipWith (*) facts [1..]

nums :: [Int]
nums = [10..]

digisFact :: Int -> Int
digisFact = sum . map ((facts !!) . digitToInt) . show

limit :: [Int] -> [Int]
limit =  takeWhile isNotMax
  where
  isNotMax = uncurry (<) . (id &&& (((facts !! 9) *) . length . show))

main = print $ sum . map fst . filter (uncurry (==)) .  map (id &&& digisFact) $ limit nums 
