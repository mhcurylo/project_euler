-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
import Data.List

main = do
  print $ length . filter isFirstSunday . takeWhile (not . is31Dec) $ allDates

isFirstSunday (Date (d, _, _, dow)) 
  | d == 1 && dow == Sunday = True
  | otherwise = False

is31Dec (Date (d, m, y, dow)) =  d == 31 && m == December && y == 2000

 
class Circular a where
  next :: a -> a

data Month = January | February | March | April | May | June | July | August | September | October | November | December 
  deriving (Show, Eq, Ord, Enum)

inc :: Int -> Int
inc = (+ 1)

instance Circular Month where
  next = (cycle [January, February, March, April, May, June, July, August, September, October, November, December] !!) . inc . fromEnum

data DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show, Eq, Ord, Enum)

instance Circular DayOfWeek where
  next = (cycle [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday] !!) . inc . fromEnum

newtype Date = Date (Integer, Month, Integer, DayOfWeek) 
  deriving (Show, Eq, Ord)

instance Circular Date where
  next (Date (d, m, y, dow))
    | nextDay == 1 && m == December = Date (nextDay, next m, y + 1, next dow)
    | nextDay == 1 = Date (nextDay, next m, y, next dow)
    | otherwise = Date (nextDay, m, y, next dow)
    where
     nextDay = if monthLength m y == d 
       then 1
       else d + 1

shorterMonths = [September, April, June, November]

divisableBy x y = y `div` x == 0

isLeapYear x = (divBy 4 || ((not $ divBy 100) || (divBy 400))) 
  where 
   divBy = divisableBy x

monthLength :: Month -> Integer -> Integer
monthLength m y 
  | m == February && isLeapYear y = 29
  | m == February && not (isLeapYear y) = 28
  | m `elem` shorterMonths = 30
  | otherwise = 31

startingDate = Date (1, January, 1900, Monday)

allDates = startingDate:map next allDates
