main::IO()
main=do
 print ((fmin (\ x -> x + 2) (\ x -> x * (-2))) 9)
 print ((fmin (+ 2) (* (-2))) 9)
 print ((fmin1 (+ 2) (* (-2))) 9)
 print (fmax (+ 18) (* 4) 5)
 print (favg (+ 15) (* 2) 7)
 print (difference (* 4.0) 4.5 5.0)
 print (lambdaTask2 0.3)
 print (composition (+ 2) (* 3) 3)
 print (comp2 (+ 2) (* 3) 3)
 print (sumOddSquares [1..5])
 print (sumOddSquares4 [1..5])
 --print (compositionOnSteroids [(+ 2) (* 3)] 5)
 
{-
  Зад. 1. Дефинирайте следните функции от по-висок ред:
  
    а) (fmin f g), която приема две едноместни числови функции f и g
    и връща едноместни числова функция, чиято стойност в точка x е
    минимума на f и g.

    б) (fmax f g), като fmin, но връща максимума на f и g.

    в) (favg f g), като fmin, но връща средното аритметично на f и g.
-}

--Зад. 1. а)
fmin :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
fmin f g = (\ x -> min (f x) (g x))

fmin1 :: (Int -> Int) -> (Int -> Int) -> Int -> Int
fmin1 f g x = min (f x) (g x)

--Зад. 1. б)
fmax :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
fmax f g = (\ x -> max (f x) (g x))

--Зад. 1. в)
favg :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
favg f g x = ((f x) + (g x)) `div` 2

{-
  Зад. 2. Да се дефинира процедура от по-висок ред (difference f a b),
  която по дадени едноаргументна реална функция f и две реални числа
  a и b намира разликата f(b) - f(a).
-}

difference :: (Double -> Double) -> Double -> Double -> Double
difference f a b = (f b) - (f a)

{-
  Зад. 3. Чрез използване на lambda израз да се дефинира процедурен обект,
  който е еквивалентен на f(x) = 2x.
-}

lambdaTask :: Double -> Double
lambdaTask x = 2 * x

lambdaTask2 :: Double -> Double
lambdaTask2 = \x -> x * 2

{-
  Зад. 4. Да се дефинира процедура от по-висок ред (composition f g), която
  намира композицията на едноаргументните реални функции f и g.
-}

composition :: (Double -> Double) -> (Double -> Double) -> (Double -> Double)
composition f g = (\x -> f (g x))

comp :: (Double -> Double) -> (Double -> Double) -> (Double -> Double)
comp f g x = f (g x)

comp2 :: (Double -> Double) -> (Double -> Double) -> (Double -> Double)
comp2 f g = f . g

{-
  Зад. 5. Напишете функция sumOddSquares :: [Int] -> Int, която намира сумата
  от квадратите на нечетните цели числа в даден списък. 
  Решете задачата по два начина - с map, filter, foldr и с list comprehension.
-}

sumOddSquares :: [Int] -> Int
sumOddSquares xs = sum (map (^ 2) (filter odd xs))

sumOddSquares2 :: [Int] -> Int
sumOddSquares2 [] = 0
sumOddSquares2 (x:xs) = 
 if x `mod` 2 == 1 then (x^2) + sumOddSquares2 xs else sumOddSquares2 xs
 
sumOddSquares3 :: [Int] -> Int
sumOddSquares3 = sum . (map (^ 2)) . (filter odd)

sumOddSquares4 :: [Int] -> Int
sumOddSquares4 xs = sum [x ^ 2 | x <- xs, odd x] 

-- map (+ 2) [1..5] връща [3,4,5,6,7]
-- filter (x `mod` 2) [1..5] връща [1,3,5]
-- foldr (+) 0 [1..5] започва от 0 и добавя следващите числа от списъка --> 0+1+2+.. 

{-
  Зад. 6. Да се дефинира процедура (composition fs), която по даден списък от
  едноаргументни реални функции fs = [f1,f2,…,fn] намира композицията на
  f1, f2, …, fn.
-}

compositionOnSteroids [(Int -> Int)] -> (Int -> Int)
compositionOnSteroids [] x = x
compositionOnSteroids fs x = ((compositionOnSteroids (init fs)) . (last fs)) x

{-
  Зад. 7. Да се дефинира процедура (hex2bin xs), която преобразува 16-тичното
  число, представено чрез списъка xs, в двоично.
  
  Пример:
    hex2bin ['1','2','3','F'] -> [[1],[1,0],[1,1],[1,1,1,1]]
    hex2bin "123F"            -> [[1],[1,0],[1,1],[1,1,1,1]]
-}