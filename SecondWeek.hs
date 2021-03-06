main :: IO()
main = do
  print "ex02_1&2.hs"
  checkEqual "countDigits 123" (countDigits 123) 3
  checkEqual "countDigits 0" (countDigits 0) 1
  checkEqual "sumDigits 6" (sumDigits 6) 6
  checkEqual "sumDigits 123" (sumDigits 123) 6
  checkEqual "pow 2 10" (pow 2 10) 1024
  checkEqual "sumDigitsIterative 123" (sumDigitsIterative 123) 6
  checkEqual "sumDigitsIterative 123" (sumDigitsIterative 0) 0
  checkEqual "reverseNumber 1234" (reverseNumber 1234) 4321
  checkEqual "reverseNumber 7" (reverseNumber 7) 7
  checkEqual "isPrime 2" (isPrime 2) True
  checkEqual "isPrime 3" (isPrime 3) True
  checkEqual "isPrime 4" (isPrime 4) False
  checkEqual "isPrime 21" (isPrime 21) False
  checkEqual "isPrime 37" (isPrime 37) True
  checkEqual "isAscending 12345" (isAscending 12345) True
  checkEqual "isAscending 12325" (isAscending 12325) False
  checkEqual "isAscending 1" (isAscending 1) True
  checkEqual "isAscending 11" (isAscending 11) True
  checkEqual "isAscending 10" (isAscending 10) False
  checkEqual "countOccurences 2 123" (countOccurences 2 123) 1
  checkEqual "countOccurences 0 1000" (countOccurences 0 1000) 3
  checkEqual "countOccurences 2 1223" (countOccurences 2 1223) 2
  checkEqual "isPerfectNumber 6" (isPerfectNumber 6) True
  checkEqual "isPerfectNumber 1" (isPerfectNumber 1) True
  checkEqual "isPerfectNumber 0" (isPerfectNumber 0) True
  checkEqual "sumPrimeDivisors 6" (sumPrimeDivisors 6) 5
  checkEqual "sumPrimeDivisors 1" (sumPrimeDivisors 1) 0
  checkEqual "sumPrimes 1 5" (sumPrimes 1 5) 10
  checkEqual "sumPrimes 0 1" (sumPrimes 0 1) 0


{-
  Зад. 1. Да се дефинира функция countDigits, която генерира линейно рекурсивен
  процес и намира броя на цифрите на дадено естествено число.
-}
countDigits :: Int -> Int
countDigits n = if n < 10 then 1 else 1 + countDigits (n `div` 10)


{-
  Зад. 2. Да се дефинира функция sumDigits, която генерира линейно рекурсивен
  процес и намира сумата от цифрите на дадено естествено число.
-}
sumDigits :: Int -> Int
sumDigits n = if n < 10 then n else n `mod` 10 + sumDigits (n `div` 10)


{-
  Зад. 3. Да се дефинира функция pow, която генерира линейно рекурсивен процес
  и намира x на степен n, където x е реално, а n - естествено число.
-}
pow :: Double -> Int -> Double
pow x n = if n == 0 then 1 else x * pow x (n - 1)


{-
  Зад. 4. Да се дефинира функция sumDigitsIterative, която генерира линейно
  итеративен процес и намира сумата от цифрите на дадено естествено число.
-}
sumDigitsIterative :: Int -> Int
sumDigitsIterative n = helper n 0
  where
    helper :: Int -> Int -> Int
    helper k res = if k == 0 then res else helper (k `div` 10) (res + k `mod` 10)


{-
  Зад. 5. Да се дефинира функция reverseNumber, която генерира линейно итеративен
  процес и по дадено естествено число n намира числото, записано със същите цифри,
  но в обратен ред.
-}
reverseNumber :: Int -> Int
reverseNumber n = helper n 0
  where
    helper :: Int -> Int -> Int
    helper k res = if k == 0 then res else helper (k `div` 10) (res * 10 + k `mod` 10)
    
{-
  Зад. 6. Да се дефинира предикат isPrime, който проверява дали дадено естествено
  число е просто.
  Забележка: Числото 1 не е нито просто, нито съставно.
-}
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = helper 2
  where
    helper :: Integer -> Bool
    helper d
      | d == n         = True
      | n `mod` d == 0 = False
      | otherwise      = helper (d + 1)


{-
  Зад. 7. Да се напише предикат isAscending, който връща истина, ако цифрите на
  дадено естествено число са в нарастващ ред от първата към последната.
-}
isAscending :: Int -> Bool
isAscending n = n < 10 || (n `mod` 10 >= ((n `div` 10) `mod` 10) && isAscending (n `div` 10))


{-
  Зад. 8. Да се напише функция countOccurences, намираща броя на срещанията на дадена
  цифра d в записа на число n.
-}
countOccurences :: Int -> Int -> Int
countOccurences d n = helper n 0
    where
    helper :: Int -> Int -> Int
    helper n count
        | n == 0 = count
        | n `mod` 10 == d = helper (n `div` 10) (count + 1)
        | otherwise       = helper (n `div` 10) count

{-
  Зад. 9. Да се напише предикат isPerfectNumber, който връща дали едно число е
  съвършено, т.е. равно на сумата от делите си.
-}
isPerfectNumber :: Int -> Bool
isPerfectNumber n = helper (n - 1) 0
  where
    helper:: Int -> Int -> Bool
    helper augN sumOfFac
        | n == 1              = True
        | n == 0 || augN == 0 = sumOfFac == n
        | n `mod` augN == 0   = helper (augN - 1) (sumOfFac + augN)
        | otherwise           = helper (augN - 1) sumOfFac


{-
  Зад. 10. Да се дефинира функция sumPrimeDivisors, която намира сумата на всички
  прости делители на едно число.
-}
sumPrimeDivisors :: Integer -> Integer
sumPrimeDivisors n = helper (n - 1) 0
  where
    helper:: Integer -> Integer -> Integer
    helper augN sumOfFacPrimes
        | (augN == 0)                       = sumOfFacPrimes
        | n `mod` augN == 0 && isPrime augN = helper (augN - 1) (sumOfFacPrimes + augN)
        | otherwise                         = helper (augN - 1) sumOfFacPrimes


{-
  Зад 11. Дефинирайте функцияга sumPrimes a b, която връща сумата на всички прости числа
  в интервала от a до b.
-}
sumPrimes :: Integer -> Integer -> Integer
sumPrimes a b = helper a 0
  where
    helper:: Integer -> Integer -> Integer
    helper curr sumOfPrimes
        | (curr - 1) == b = sumOfPrimes
        | isPrime curr    = helper (curr + 1) (sumOfPrimes + curr)
        | otherwise       = helper (curr + 1) sumOfPrimes

-- Code below is needed for test purposes
checkEqual :: (Eq a, Show a) => String -> a -> a -> IO()
checkEqual preface actual expected =
  print ((if null preface then "" else preface ++ ": ") ++
    if (actual /= expected)
    then  "expected: " ++ show expected ++ ", but got: " ++ show actual
    else "OK, result: " ++ show actual
  )