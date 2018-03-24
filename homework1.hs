main::IO()
main = do
 print (solveQuadratic 1 4 2)
 --print (solveQuadratic 1 2 5) Must return error
 print (sumPrimes 1 3)
 print (countPalindromes 6 12)
 print (truncatablePrime 4)
 print (truncatablePrime 3797)
 print (truncatablePrime 47)

{-
Задача 1. ​Нека е дадено квадратно уравнение аx x , където a, b и c са реални 2 + b + c = 0
числа. Дефинирайте solveQuadratic :: Double -> Double -> Double -> (Double, Double)​,
която получава като аргументи, коефициентите a, b и c и връща двойката решения на
уравнението или грешка, ако дискриминантата е отрицателна.
-}

solveQuadratic :: Double -> Double -> Double -> (Double, Double)
solveQuadratic a b c = if d < 0 then error "0" else (x1,x2)
 where
  d = b*b - 4*a*c
  e = (-b) / (2*a)
  x1 = (-b + sqrt d)/(2*a)
  x2 = (-b - sqrt d)/(2*a)
  
{-
Задача 2. Дефинирайте функцията sumPrimes :: Integer -> Integer -> Integer​, която
приема целите числа n и k и връща сбора първите k прости числа по-големи или равни
на n.
-}

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = null [d | d <- [2..(n `div` 2)], n `mod` d == 0]

sumPrimes :: Integer -> Integer -> Integer
sumPrimes n 0 = 0
sumPrimes n k = 
 if (n < 2) then sumPrimes (n + 1) k 
 else if (isPrime n) then n + (sumPrimes (n + 1) (k - 1))
 else (sumPrimes (n + 1) k)
 
 {-
 Задача 3. Ще наричаме едно цяло положително число палиндром, ако то е равно на
числото записано със същите цифри, но в обратен ред (приемаме, че числата са
дефинирани в десетична бройна система).
Дефинирайте функцията countPalindromes :: Integer -> Integer -> Integer​, която
приема аргументи a и b и връща броя на числата палиндроми в целочисления интервал
[a, b], a ≤ b .
 -}

reverseNumber :: Int -> Int
reverseNumber n = helper n 0
  where
    helper :: Int -> Int -> Int
    helper k res = if k == 0 then res else helper (k `div` 10) (res * 10 + k `mod` 10)
    
 
countPalindromes :: Int -> Int -> Int 
countPalindromes a b 
  | a > b = 0
  | a < 10 = 1 + (countPalindromes (a + 1) b)
  | (reverseNumber a) == a = 1 + (countPalindromes (a + 1) b)
  | otherwise = (countPalindromes (a + 1) b)


{-
Задача 4. Дефинирайте предикат truncatablePrime :: Integer -> Bool​, който връща
стойност True точно когато аргументът притежава едновременно следните свойства:
● Числото е просто
● Всички числа, които се получават с премахване на цифри в края на числото
също са прости
Пример за такова число е 3797, тъй като 3797, 379, 37 и 3 са прости.
truncatablePrime 3797 -> True
truncatablePrime 47 -> False
-}

truncatablePrime :: Integer -> Bool
truncatablePrime n 
 | ((isPrime n) && (n < 10)) = True
 | (isPrime n) = (truncatablePrime (n `div` 10))
 | otherwise = False