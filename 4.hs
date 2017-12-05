import Control.Applicative
import Data.List

threeDig = [999,998..100]
isPalindrom x = looks == reverse looks 
  where
   looks = show x

main = print $ head . reverse . sort . filter isPalindrom . map (uncurry (*)) $ liftA2 (,) threeDig threeDig

