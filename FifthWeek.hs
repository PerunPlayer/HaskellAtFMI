main::IO()
main = do
 print (takingNumber 3 22543)
 print (takingNumber 3 22000)
 print (digitsN 456456)
 print (merge [1,3,4,6,6,7] [2,4,4,8])
 print (ordered [1,2,5,3,4,2])
 print (ordered [1,2,4,6,8])
 print (prime 9)
 print (prime 23)

{-
  Зад. 1. Да се дефинира функция която взима числото, което се образува
  от последните n цифри на числото m.
-}

takingNumber :: Int -> Int -> Int
takingNumber n m = m `mod` (10 ^ n)

{-
  Зад. 2. Да се дефинира функция digits n, която връща списък с цифрите на
  цялото число n >= 0.
-}

digitsN :: Int -> [Int]
digitsN n = if n < 10 then [n] else digitsN(n `div` 10) ++ [n `mod` 10] 

{-
  Зад. 3. Нaпишете функцията merge xs ys, която приема два списъка подредени
  в нарастващ ред и ги обединява в един списък, чийто елементи също са подредени
  в нарастващ ред.
-}

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge x1s@(x:xs) y1s@(y:ys) = 
 if x < y then x : (merge xs y1s) else y : (merge x1s ys)

{-
  Зад. 4. Напишете функция ordered :: (Ord a) => [a] -> Bool, която проверява
  дали елементите на даден списък са подредени в нарастващ ред.
-}

ordered :: (Ord a) => [a] -> Bool
ordered [] = True
ordered [_] = True
ordered (y:x:xs) = y < x && ordered (x:xs)

{-
  Зад. 5. Напишете функция different :: (Eq a) => [a] -> Bool, която проверява 
  дали един списък се състои от различни елементи.
-}

--count :: Eq a => a -> [a] -> Int
--count x xs = lenght [p | p <- xs, p == x]

--different :: (Eq a) => [a] -> Bool
--different xs = null [p | p <- xs, count p xs > 1]

{-
  Зад. 6. Напишете функцията prime :: Int -> Bool, която проверява дали едно число
  е просто с помощта на list comprehension.
-}

prime :: Int -> Bool
prime 1 = False
prime n = null [d | d <- [2..(n `div` 2)], n `mod` d == 0]

{-
  Зад. 7. Дефинирайте следните функции от по-висок ред:
  
    а) (fmin f g), която приема две едноместни числови функции f и gh
    и връща едноместни числова функция, чиято стойност в точка x е
    минимума на f и g.

    б) (fmax f g), като fmin, но връща максимума на f и g.

    в) (favg f g), като fmin, но връща средното аритметично на f и g.
-}

{-
  Зад. 8. Напишете функция sumOddSquares :: [Int] -> Int, която намира сумата от
  квадратите на нечетните цели числа в даден списък.
  Решете задачата по два начина - с map, filter, foldr и с list comprehension
-}