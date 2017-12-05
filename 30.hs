import Control.Arrow 
import Data.List
import Data.Ord

sumFives :: Int-> Int
sumFives =  sum . map ((^5) . read . (:[])) . show

al :: [Int]
al = [10..]

mx :: Int
mx = head $ dropWhile (\x -> x < (9^5 * length (show x))) al 

main = print $ sum . map fst . filter (uncurry (==)) $ map (id &&& sumFives) $ [10..mx]
