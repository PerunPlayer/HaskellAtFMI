main :: IO()
main = do
  print "ex03_1&2.hs"
  checkEqual "OperatorOvr" (12 ## 2) 5
  checkEqual "OperatorOvr" (17 ## 3) 344
  checkEqual "IsNarcissistic" (isNarcissistic 153) True
  checkEqual "IsNarcissistic" (isNarcissistic 370) True
  checkEqual "IsNarcissistic" (isNarcissistic 371) True
  checkEqual "IsNarcissistic" (isNarcissistic 372) False
  checkEqual "isAutomorphic" (isAutomorphic 25) True
  checkEqual "isAutomorphic" (isAutomorphic 4) False
  checkEqual "isAutomorphic" (isAutomorphic 0) True
  checkEqual "isAutomorphic" (isAutomorphic 1) True
  checkEqual "calcSum" (calcSum 2 0) 1
  checkEqual "calcSum" (calcSum 2 1) 3
  checkEqual "calcSum" (calcSum 2 3) 15
  print (getInterval 1 15)
  print (sumElems [1,2,3,4,5])
  print (countElems [1,2,3])
  print (memberOf 3 [2,4,5,1,3])
  print (removeFirst 3 [2,3,4,3,5])
  print (removeFirst 3 [3,2,4,3,5])
  print (countOccurences 3 [3,2,4,3,5])
  print (rev [1,2,3,4,455,56,7,8,8])
  print (rev [])
  


{-
  Зад. 1. Напишете оператор n ## k, който приема n > 0 и k >= 0 и връща сумата
  от всяка цифра на n повдигната на степен k.
  Примери:
    12 ## 2 = 1 ^ 2 + 2 ^ 2 = 1 + 4   = 5
    17 ## 3 = 1 ^ 3 + 7 ^ 3 = 1 + 343 = 344
-}
(##) :: Int -> Int -> Int
n ## k = if n < 10 then n ^ k else (mod n 10) ^ k + (div n 10) ## k


{-
  Зад. 2. Да се дефинира функцията isNarcissistic n, която приема като аргумент
  цялото положително число n и връща дали то е нарцистично. Нарцистични се наричат
  числата, които са равни на сбора на цифрите си (в десетична бройна система),
  всяка повдигната на степен броя на цифрите на числото.
  Пример за такова число е 153,
  тъй като 1 ^ 3 + 5 ^ 3 + 3 ^ 3 = 1 + 125 + 27 = 153.
-}

countDigits :: Int -> Int
countDigits n = if n < 10 then 1 else 1 + countDigits (div n 10)

isNarcissistic :: Int -> Bool
isNarcissistic n = n == n ## countDigits n


{-
  Зад. 3. Да се дефинира предикат isAutomorphic, който приема число n и
  проверява дали n^2 завършва с цифрите на n.
-}
isAutomorphic :: Int -> Bool
isAutomorphic n = n == mod (n * n) (10 ^ countDigits n)


{-
  Зад. 4. По зададени x и n, да се изчисли сумата: 1 + x + x^2 + x^3 + ... + x^n.
-}
calcSum :: Double -> Int -> Double
calcSum x n = if n == 0 then 1 else x ^ n + calcSum x (n - 1)


{-
  Зад. 5. Да се реши задача 4, чрез използване на не повече от n умножения.
-}
--calcSumFast :: Double -> Int -> Double
--calcSumFast x n = undefined


{-
  Зад. 6. Да се напише функция, която връща като списък целите числа в 
  зададен интервал [a, b].
-}
getInterval :: Int -> Int -> [Int]
getInterval a b = 
 if a > b then [] else a : getInterval (a + 1) b


{-
  Зад. 7. Напишете фукнция, която намира сумата на елементите на списък от числа.
-}
sumElems :: [Int] -> Int
sumElems xs = 
 if null xs then 0 else head xs + sumElems (tail xs)
 
sumElems2 :: [Int] -> Int
sumElems2 [] = 0
sumElems2 (x:xs) = x + sumElems2 xs


{-
  Зад. 8. Напишете фунция, която намира броя на елементите на списък.
-}
countElems :: [t] -> Int
countElems xs = 
 if null xs then 0 else 1 + countElems (tail xs)
 
--countElems2 xs = lenght xs


{-
  Зад. 9. Напишете предикат, който проверява дали даден елемент се среща в списък.
-}
memberOf :: Int -> [Int] -> Bool
memberOf x xs  
 | head xs == x = True
 | null xs = False
 | otherwise = memberOf x (tail xs)


{-
  Зад. 10. Напишете функция, която премахва първото срещане на x в списъка xs.
-}
removeFirst :: Int -> [Int] -> [Int]
removeFirst x xs = 
 if x == head xs then tail xs else head xs : (removeFirst x (tail xs))


{-
  Зад. 11. Напишете функция, която връща броя на срещанията на х в списъка хs.
-}
countOccurences :: Int -> [Int] -> Int
countOccurences x xs 
 | null xs = 0
 | otherwise = (if head xs == x then 1 else 0) + countOccurences x (tail xs)

{-
  Зад. 12. Напишете функция, която обръща даден списък хs.
-}

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]





-- Code below is needed for test purposes
checkEqual :: (Eq a, Show a) => String -> a -> a -> IO()
checkEqual preface actual expected =
  print ((if null preface then "" else preface ++ ": ") ++
    if (actual /= expected)
    then  "expected: " ++ show expected ++ ", but got: " ++ show actual
    else "OK, result: " ++ show actual
  )