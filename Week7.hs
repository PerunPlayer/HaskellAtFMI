import Data.List

main :: IO()
main = do
  print (isHeterogram "YuGiOh!")
  print (isHeterogram "giga gagagigo")
  print (vectSum [1, 2, 3] [4, 5, 6])
  print (scalarProd [1, 2, 3] [4, 5, 6])
  print (getDecreasing [[5, 1, 2, 3, 4], [1, 1, -1, -2, -3], [4, 3, 2, 1], [7, 6, 5]])
  --print (closestToAverage [(5, 18.5),(15, 13.2),(10, 16.8),(11, 22.8),(3, 14.6)])
  print ((maxSquare [sqrt, (\x -> x + 2), (\x -> 2 * x)]) 5)
  print (mostPopular [("Mouse", 3), ("Keyboard", 3), ("Monitor", 2), ("Monitor", 4), ("Mouse", 1)])
  print (zeta 5 1.5)
  print (coldestMonth [(1, 1, 0), (1, 10, -5), (1, 20, 8), (2, 1, 0), (2, 10, -5), (2, 20, 0), (3, 1, 5), (3, 10, 10), (3, 20, 8), (4, 1, 9), (4, 10, 15), (4, 20, 18)])

{-
  Зад. 1. Хетерограми: дефинирайте функцията isHeterogram str, която проверява дали
  символният низ str e хетерограма. Хетерограма се нарича символен низ, в който всеки
  символ се среща само по веднъж.

  Примери:
    isHeterogram "abcd" = True
    isHeterogram "abbd" = False
-}
isHeterogram :: String -> Bool
isHeterogram "" = True
isHeterogram (c:cs) = not (c `elem` cs) && isHeterogram cs
 

{-
  Зад. 2.
    a). Дефинирайте функцията vectSum xs ys, която връща сбора на векторите xs и ys.

    Примери:
      vectSum [1, 2, 3] [4, 5, 6] = [5, 7, 9]

    б). Дефинирайте функцията scalarProd xs ys, която връща скаларното произведение на
    векторите xs и ys.

    Примери:
      scalarProd [1, 2, 3] [4, 5, 6] = 32
-}
vectSum :: Num a => [a] -> [a] -> [a]
vectSum [] _ = []
vectSum (x:xs) (y:ys) = (x + y) : (vectSum xs ys)

scalarProd :: Num a => [a] -> [a] -> a
scalarProd [] _ = 0
scalarProd (x:xs) (y:ys) = (x * y) + (scalarProd xs ys)

{-
  Зад. 3. Напишете функция getDecreasing, която за даден списък xs, елементите на който са
  непразни списъци от числа, връща като резултат списък от тези елементи на xs, които
  представляват строго намаляваща редица.

  Пример:
   getDecreasing [[5, 1, 2, 3, 4], [1, 1, -1, -2, -3], [4, 3, 2, 1], [7, 6, 5]] 
   -> [[4, 3, 2, 1], [7, 6, 5]]
-}

getDecreasing :: Ord a => [[a]] -> [[a]]
getDecreasing xss = filter isDecreasing xss

isDecreasing :: Ord a => [a] -> Bool
isDecreasing [] = True
isDecreasing [_] = True
isDecreasing (y:z:ys) = y > z && isDecreasing (z:ys)

--isDecreasing2 ys = all (zipWith (>) ys (tail ys))
{-isDecreasing2 [5,4,3,2] -> zipWith (>) [5,4,3,2] [4,3,2] -> 
 all [(5 > 4), (4 > 3), (3 > 2)] -> all [True, True, True] -> True -}

{-
  Зад. 4. Температурно измерване се описва с типа
  type Measuring = (Int, Float),
  където стойността от тип Int задава ден от месеца, а стойността от тип Float - измерена
  температура за този ден. Напишете функция closestToAverage :: [Measuring] -> Int , която по
  списък от температурни измервания намира деня, в който измерената температура е най-близо
  до средната температура през месеца.

  Примери :
    closestToAverage [(5, 18.5),(15, 13.2),
                      (10, 16.8),(11, 22.8),
                      (3, 14.6)] → 10 (средната температура е 17.18)

    closestToAverage [(1, 23.6),(6, 24.2),
                      (11, 24.2),(16, 21.2),
                      (21, 23.8),(26, 26.5),
                      (31, 24.5)] → 6 или 11 или 21 (средната температура е 24.0)
-}
type Measuring = (Int, Float)
m :: Measuring
m = (1, 2.2)

{-closestToAverage :: [Measuring] -> Float
closestToAverage xs =
 fst (foldl1 (\ currMeasuring@(_, currTemp) bestMeasuring@(_, bestTemp) ->
              if abs (currTemp - avgTemp) < abs (bestTemp - avgTemp) then currMeasuring else bestMeasuring) xs)
    where avgTemp xs = sum [q | (_, q) <- xs] / (fromIntegral (length xs))-}

{-
  Зад. 5. Дефинирайте функция maxSquare xs, където xs е непразен списък от
  едноаргументни числови функции. Оценката на обръщението към функцията да е
  числова функция на един аргумент x, която дава стойността f(x) на тази
  функция f от списъка xs, за която числото f(x)^2 е най-голямо.
  
  Пример: (maxSquare [sqrt, (\x -> x + 2), (\x -> 2 * x)]) 5 -> 10
-}
maxSquare :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
maxSquare xs = \z -> (foldl1 (\ currf bestf -> if (currf z)^2 > (bestf z)^2 then currf else bestf) xs) z

{-
  Зад. 6. Поръчка се описва с типа type Order = (String, Int) , като стойността
  от тип String задава името на продукт, а стойността от тип Int - поръчаното
  количество от този продукт. Напишете функция mostPopular :: [Order] -> String,
  която по даден списък от поръчки намира името на продукта, от който са поръчани
  най-много бройки.

  Примери :
    mostPopular [("Mouse", 3), ("Keyboard", 3),
                 ("Monitor", 2), ("Monitor", 4),
                 ("Mouse", 1)] → "Monitor"

    mostPopular [("Mouse", 3), ("Keyboard", 3),
                 ("Monitor", 2), ("Monitor", 4),
                 ("Web camera", 4),("Keyboard", 1)]
    → "Monitor" или "Web camera"
-}

type Order = (String, Int)
o :: Order
o = ("Mouse", 3)

mostPopular :: [Order] -> String
mostPopular xs = snd (maximum [((sum [c | (n, c) <- xs, n == name]), name) | name <- (nub (map fst xs))])

--fst :: (a, b) -> a
--snd :: (a, b) -> b
--trd :: (a, b, c) -> c

{-
Задача 1. Дефинирайте функцията zeta n x, която приема целочисления аргумент n и реалното число x
и връща сбора на първите n члена на редицата 1, 1 / (2 ^ x), .. 1 / (k ^ x) ..
-}
zeta :: Int -> Double -> Double
zeta n x = sum [1 / ((fromIntegral i) ** x) | i <- [1..n]]

{-
Задача 2. Нека са дадени множествата xs и ys, и едноаргументата функция f от xs към ys.
Ще наричаме множеството ys образ на xs през f, тогава и само тогава когато за всяко x от xs,
f(x) принадлежи на ys и за всяко y от ys, съществува x, елемент на xs, за който f(x) = y.

Дефинирайте функцията isImageOf f xs ys, която връща дали ys е образ на xs през f.
-}
isImageOf :: Eq b => (a -> b) -> [a] -> [b] -> Bool
isImageOf f xs ys = (and (map (`elem` ys) vs)) && (and (map (`elem` vs) ys))
 where vs = [f x | x <- xs]
 
{-
Задача 3. Нека са дефинирани следните типове:
type Temperature = Float
type Day = Int
type Month = Int
type Record = (Month, Day, Temperature)

Дефинирайте функцията coldestMonth records, която приема списък от температурни измервания
и връща, кой е бил най-студения месец (т.е. месецът с най-ниска средна температура)
Пример:
    coldestMonth [(1, 1, 0), (1, 10, -5), (1, 20, 8), (2, 1, 0), (2, 10, -5), (2, 20, 0),
                  (3, 1, 5), (3, 10, 10), (3, 20, 8), (4, 1, 9), (4, 10, 15), (4, 20, 18)]
            -> 2
-}
type Temperature = Float
type Day = Int
type Month = Int
type Record = (Month, Day, Temperature)

coldestMonth :: [Record] -> Month
coldestMonth records = snd (minimum mAvg)
 where
  months = nub [m | (m, _, _) <- records]
  mDt = [(m, [t | (m2, _, t) <- records, m2 == m]) | m <- months]
  mAvg = [((sum ts / (fromIntegral (length ts))), m) |(m, ts) <- mDt]
