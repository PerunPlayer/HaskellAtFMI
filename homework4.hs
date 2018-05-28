main :: IO()
main = do
 print (findAvg [2.3, 4.5, 7.0, 9.9, 6.5])
 print (closestToAverage [(Temp 1 23.6),(Temp 6 24.2),(Temp 11 24.2),(Temp 16 21.2),(Temp 21 23.8),(Temp 26 26.5),(Temp 31 24.5)])
 
{-
Температурно измерване се описва с типа data Measuring = Temp Int Float, където стойността от тип Int задава ден
от месеца, а стойността от тип Float – измерена температура за този ден. Дефинирайте функция 
closestToAverage :: [Measuring] -> Int, която по списък от температурни измервания намира деня, в който измерената 
температура е най-близо до средната температура през месеца.
Пример:
closestToAverage [(Temp 1 23.6),(Temp 6 24.2),
 (Temp 11 24.2),(Temp 16 21.2),
 (Temp 21 23.8),(Temp 26 26.5),
 (Temp 31 24.5)] → 6 или 11 или 21
 (средната температура е 24.0)
-}

data Measuring = Temp Int Float

findAvg :: [Float] -> Float
findAvg [] = 0
findAvg temps = (sum temps) / (fromIntegral (length temps))

getDay :: Measuring -> Int
getDay (Temp day temperature) = day

closestToAverage :: [Measuring] -> Int
closestToAverage [] = 0
closestToAverage measurings = 
  getDay (foldl1 (\ currMeasuring@(Temp _ currTemp) bestMeasuring@(Temp _ bestTemp) ->
      if abs (currTemp - avgTemp) < abs (bestTemp - avgTemp) then currMeasuring else bestMeasuring) measurings)
          where avgTemp = findAvg [t | (Temp d t) <- measurings]

{-
Нека за представянето на двоично дърво от цели числа се използва алгебричен тип със следната дефиниция:
data BTree = Empty | Node Int BTree Btree. Да се дефинира функция (grandchildrenIncreased tree), която проверява
дали всеки връх на двоичното дърво tree е поне с единица по-голям от своя дядо (ако има такъв).
-}
--Функция, която проверява дали всеки връх на двоичното дърво е поне с 1 по-голям от своя дядо (ако има такъв).

data BTree = Empty | Node Int BTree Btree

--access ancestor

treeDepth :: (Num b, Ord b) => Tree a -> b
treeDepth (Node value (tr1) (tr2)) = 1 + max (treeDepth tr1) (treeDepth tr2)

grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased Empty = False
grandchildrenIncreased tree = 
  | (treeDepth tree == 1) || (treeDepth tree == 2) = True
  | otherwise = 
