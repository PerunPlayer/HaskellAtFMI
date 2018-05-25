{- Асоциативни списъци и графи. Представяне и пътища в граф. -}
import Data.List

{-
Задача 1. Нека е даден списък от двойки edges, съответстващ на ребрата на даден
ориентиран граф. Дефинирайте следните функции:

а). nodes edges, която връща списъкс с нодовете на съответния граф.
б). neighbours edges node, която връща списък със съседите на даден връх.
в). adjacencyList edges, която връща списъка на наследниците на съответния граф.

Примери:
    nodes [(1, 2), (1, 3), (2, 3), (2, 4)] -> [1, 2, 3, 4]

    neighbours [(1, 2), (1, 3), (2, 3), (2, 4)] 2 -> [3, 4]
    neighbours [(1, 2), (1, 3), (2, 3), (2, 4)] 4 -> []

    adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)] -> [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]
-}
nodes :: (Eq a) => [(a, a)] -> [a]
nodes edges = nub ((uncurry (++)) (unzip edges))

--nodes2 = nub . (uncurry (++)) . unzip

neighbours :: (Eq a) => [(a, a)] -> a -> [a]
neighbours edges node = [v2 | (v1, v2) <- edges, v1 == node]

adjacencyList :: (Eq a) => [(a, a)] -> [(a, [a])]
adjacencyList edges = [(v, neighbours edges v) | v <- nodes edges]

{-
Задача 2. Дефинирайте функцията isPath adjs nodes, която приема списък на наследниците
на даден граф adjs и списък от върхове nodes и връща дали nodes е път в съответния граф.

Примери:
    isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 2, 4] -> True
    isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 3, 4] -> False
-}
isPath :: (Eq a) => [(a, [a])] -> [a] -> Bool
isPath adjs [] = False
isPath adjs nodes
 | (length nodes == 1) && (elem (head nodes) [node | (node, children) <- adjs]) = True
 | (elem (head nodes) [node | (node, children) <- adjs]) && (elem (nodes !! 1) (concat [children | (node, children) <- adjs, node == (head nodes)])) = isPath adjs (tail nodes)
 | otherwise = False

{-
Задача 3. Нека е дадено двойчно дърво, представено като списък от тройки,
чиито първи елемент е идентификатор на дадения връх, а 2-рият и 3-тият,
идентификаторите на съответно лявото и дясно дете на върха.

Дефинирайте функцията listLeaves nodes, която връща списък с всички листа
на даденото дърво.

Примери:
    listLeaves [(1, 2, 3), (2, 4, 5)] -> [4, 3, 5]
    listLeaves [(2, 4, 5), (1, 2, 3)] -> [4, 5, 3]
-}

tupleToList :: (Eq a) => [(a,a)] -> [a]
tupleToList ((a,b):xs) = a : b : tupleToList xs
tupleToList _ = []

listLeaves :: (Eq a) => [(a, a, a)] -> [a]
listLeaves [] = []
listLeaves nodes = leaves \\ vertexes
 where leaves = tupleToList (map sndthd nodes)
       vertexes = map first nodes
       sndthd (_, leaf1, leaf2) = (leaf1, leaf2)
       first (v, _, _) = (v)




-- ТЕСТОВЕ НА ЗАДАЧИТЕ --
main = do
    print $ nodes [(1, 2), (1, 3), (2, 3), (2, 4)]
    print $ neighbours [(1, 2), (1, 3), (2, 3), (2, 4)] 2
    print $ neighbours [(1, 2), (1, 3), (2, 3), (2, 4)] 4
    print $ adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)]

    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 2, 4]
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 3, 4]

    print $ listLeaves [(1, 2, 3), (2, 4, 5)]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)]