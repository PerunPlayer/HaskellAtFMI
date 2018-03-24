main :: IO()
main = do
    print (mymin 6 2)
    print (mymax 2 1)
    print (myfunc 2 4)
    print (myfib 8)
    print (myfib 0)
    print (mygcd 176 88)
    print (mymaxdivisor 64)
    print (isInside 6 1 5)
    print (isInside 2 1 5)
    print (isLeapYear 2008)
    print (isLeapYear 2018)
    print (isValidDate 29 2 2004)
    print (isValidDate 29 2 2005)
    print (isValidDate 30 2 2004)
    print (isValidDate 31 6 2018)
    print (isValidDate 31 5 2018)
    print (isValidDate 4 3 2018)

{-
Задача 1. Да се напише функция mymin, която приема два аргумента и връща по-малкият от тях.
-}
mymin :: Int -> Int -> Int
mymin a b = if a < b then a else b

{-
Задача 2. Да се напише функция mymax, която приема два аргумента и връща по-големият от тях.
-}
mymax :: Int -> Int -> Int
mymax a b = if a > b then a else b

{-
Задача 3. Да се напише функция myfunc, която пресмята на средно аритметичното от квадратите на 2 числа.
-}
myfunc :: Double -> Double -> Double
myfunc a b = (a*a + b*b)/2

{-
Задача 4. Да се напише myfib, която получава един аргумент n и връща n-тото число на Фибоначи.
(Заб.: редицата е 1, 1, 2, 3, 5, ... и е индексирана от 0.)

Да се напише и итеративно решение.
-}
myfib :: Integer -> Integer
myfib n = 
 if n == 0 then 1
 else if n == 1 then 1
 else myfib(n-1) + myfib(n-2)

myfibIterative :: Integer -> Integer
myfibIterative n = undefined

{-
Задача 5. Да се напише функция mygcd a b, която връща НОД(a, b).
-}
mygcd :: Int -> Int -> Int
mygcd a b = 
 if b == 0 then a
 else mygcd b (a `mod` b)

{-
Задача 6. Да се напише функция mymaxdivisor x, която намира най-големия делител d на цялото число x > 1,
за който d < x.
-}
mymaxdivisor :: Int -> Int
mymaxdivisor x = go (x-1) x
 where
  go :: Int -> Int -> Int
  go n x
   | x `mod` n == 0 = n
   | otherwise = go (n-1) (x)


{-
Задача 7. Да се дефинира функцията isInside x a b, която проверява дали числото x се намира
в затворения интервал [a .. b].
-}
isInside :: Integer -> Integer -> Integer -> Bool
isInside x a b = (x >= a && x <= b)


{-
Задача 8. Да се дефинира функцията isLeapYear year, която проверява дали годината year
е високосна.
-}
isLeapYear :: Integer -> Bool
{-isLeapYear year
 | isDivBy 400 = True
 | isDivBy 100 = False
 | isDivBy 4   = True
 | otherwise   = False
 where 
  isDivBy :: Integer -> Bool
  isDivBy a = mod year a == 0
-}
isLeapYear year
 | isDivBy 400 = True
 | isDivBy 100 = False
 | isDivBy 4   = True
 | otherwise   = False
 where isDivBy a = mod year a == 0

{-
Задача 9. Да се дефинира функцията isValidDate day month year, която връща дали датата
(day, month, year) e валидна.
-}
isValidDate :: Integer -> Integer -> Integer -> Bool
isValidDate day month year
  | month == 1 ||
    month == 3 ||
    month == 5 ||
    month == 7 ||
    month == 8 ||
    month == 10 ||
    month == 12     = 1 <= day && day <= 31
  | month == 2      = 1 <= day && day <= 28 || (day == 29 && isLeapYear year)
  | otherwise       = 1 <= day && day <= 30