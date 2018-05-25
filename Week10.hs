import Data.List

main :: IO()
main = do
 print (addMatrices [[1,2],[3,4]] [[4,3],[2,1]])
 print (nullify [[1,2,9,0],[3,6,3,4],[3,9,4,3],[2,0,3,1]])
 print (myTranspose [[1,2],[5,6],[3,4]])
 print (multMat [[1,2],[2,1]] [[3,4],[4,3]])
 print (countEdges [[1,2,3,4],[2,1,3],[3,1,2],[4,1]])
 print (leafsOfMatrix [[1,2,3,4],[2,1,3],[3,1,2],[4,1]])
 
{-
  Зад. 1. Да се напише функция, която намира сбора на две матрици, представени
  като списък от списъци.
-}

addMatrices :: (Num a, Eq a) => [[a]] -> [[a]] -> [[a]]
addMatrices [] [] = []
addMatrices (m:ms1) (n:ms2) = zipWith (+) m n : addMatrices ms1 ms2

{-
  Зад. 2. Да се напише функция, която нулира всички сълбове на матрица, в които
  се съдържа стойност 0. Матрицата е представена като списък от списъци.
-}

cleanCol :: (Num a, Eq a) => [a] -> [a]
cleanCol xs = if elem 0 xs then map (*0) xs else xs

nullify :: (Num a, Eq a) => [[a]] -> [[a]]
nullify matrix@([]:_) = matrix
nullify matrix = zipWith (\ x xs -> x:xs) (cleanCol (map head matrix)) (nullify (map tail matrix))

{-
  Зад. 3. Напишете функция която намира транспонираната на дадена матрица.
-}

myTranspose :: [[Int]] -> [[Int]]
myTranspose [] = []
myTranspose ([] : xss) = myTranspose xss
myTranspose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : myTranspose (xs : [ t | (_:t) <- xss]) 

{-
  Зад. 4. Напишете функция, която намира произведението на две матрици.
-}

multMat :: Num a => [[a]] -> [[a]] -> [[a]]
multMat xss yss = [[sum (zipWith (*) xs ys) | ys <- (transpose yss)] | xs <- xss]

{-
  Зад. 5. Напишете функция, която намира броя на дъгите на граф, представен
  чрез матрица на съседство.
-}

countEdges :: [[Int]] -> Int
countEdges graph = sum (map sum graph)

{-
  Зад. 6. Напишете функция, която намира всички листа на дърво, представено
  чрез матрица на съседство.
-}

leafsOfMatrix :: [[Int]] -> [Int]
leafsOfMatrix xss = [ind | (ind, edgesCount) <- zip [0..] (map sum xss), edgesCount == 0]