import Control.Arrow ((&&&))
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

nums = [1..1000000]

isPalindrom :: String -> Bool
isPalindrom = uncurry (==) . (id &&& reverse)

isPalindrom10 :: Int -> Bool
isPalindrom10 = isPalindrom . show

isPalindrom2 :: Int -> Bool
isPalindrom2 x = isPalindrom $ showIntAtBase 2 intToDigit x ""

nonDivBy x = (/= 0) . (`mod` x)

fnonDivBy x= filter (nonDivBy x)

main = print $ sum . filter isPalindrom10 . filter isPalindrom2 . fnonDivBy 10 . fnonDivBy 2 $ nums

