{- Алгебричин типове -}
{-
data Bool = False | True
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
-}

{- ПРИМЕРИ -}
{- 
Пример 1. Нека е дефиниран алгебричният тип Point2D, описващ точка в 2D пространството.
Да се дефинира функцията distance, която приема две точки и връща разстоянието между тях.

Примери:
    distance (Point2D 1 1) (Point2D 2 2)
-}
data Point2D = Point2D Double Double
    deriving (Read, Show)

distance :: Point2D -> Point2D -> Double
-- използваме съпоставяне по шаблон, с по един шаблон за всеки от случайте
distance (Point2D x1 y1) (Point2D x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2


{- Дефиниция на полиморфния алгебричен тип List, описващ едносвързан списък (подобно на типа [a]). -}
data List a = Nil | a `Cons` List a
    deriving (Read, Show)

{-
Пример 2. Да се дефинира полиморфната функция  listAppend xs x, която приема списък xs от тип List a
и обект x от тип а и  добавя x в края на xs.   

Примери: 
    listAppend (1 `Cons` (2 `Cons` (3 `Cons` Nil))) 10 = 1 `Cons` (2 `Cons` (3 `Cons` (10 `Cons` Nil)))
    listAppend (1 `Cons` (2 `Cons` (3 `Cons` Nil))) 42 = 1 `Cons` (2 `Cons` (3 `Cons` (42 `Cons` Nil)))
-}
listAppend :: List a -> a -> List a
-- използваме съпоставяне по шаблон, с по един шаблон за всеки от случайте
listAppend Nil x            = x `Cons` Nil
listAppend (h `Cons` ts) x  = h `Cons` (listAppend ts x)


{- Дефиниция на типа Tree a описващ произволно двоично дърво. -}
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Read, Show)

{-
Пример 3. Да се дефинира функцията treeEmpty tree, която връща дали двоичното
дърво tree е празно.
-}
treeEmpty :: Tree a -> Bool
-- използваме съпоставяне по шаблон, с по един шаблон за всеки от случайте
treeEmpty Empty = True
treeEmpty _ = False

{-
Пример 4. Да се дефинира функцията treeRoot tree, която връща корена на
двоичното дърво tree.
-}
treeRoot :: Tree a -> a
treeRoot Empty = error "empty tree!"
treeRoot (Node val _ _) = val

{-
Пример 5. Да се дефинира функцията treeCount tree, която връща броя на
елементите на двоичното дърво tree.
-}
treeCount :: (Num b) => Tree a -> b
treeCount Empty = 0
treeCount (Node _ left right) = 1 + treeCount left + treeCount right


{- ЗАДАЧИ -}

{-
Задача 1. Нека е даден полиморфният алгебричен тип List (дефиниран в пример 1). 
Дефинирайте (рекурсивно) следните функции:
а). mkList, която приема стандартен списък и го превръща в списък от тип List.
б). unList, която приема списък от тип List и го превръща в стандартен списък.
в). listEmpty, същата като empty, но за списък от тип List.
г). listHead, същата като head, но за списък от тип List.
д). listTail, същата като tail, но за списък от тип List.
е). listMap, същата като map, но за списък от тип List.
ж). listFilter, същата като filter, но за списък от тип List.

Примери:
    mkList [1, 2, 3] = 1 `Cons` (2 `Cons` (3 `Cons` Nil))
    unList (1 `Cons` (2 `Cons` (3 `Cons` Nil))) = [1, 2, 3]
    listEmpty (1 `Cons` (2 `Cons` (3 `Cons` Nil))) = False
    listHead (1 `Cons` (2 `Cons` (3 `Cons` Nil))) = 1
    listTail (1 `Cons` (2 `Cons` (3 `Cons` Nil)))  = 2 `Cons` (3 `Cons` Nil)
    listMap (+1) (1 `Cons` (2 `Cons` (3 `Cons` Nil))) = 2 `Cons` (3 `Cons` (4 `Cons` Nil))
    listFilter even (1 `Cons` (2 `Cons` (3 `Cons` Nil))) = 2 `Cons` Nil
-}
-- 1.а.
mkList :: [a] -> List a
mkList [] = Nil
mkList (x:xs) = Cons x (mkList xs)

-- 1.б.
unList :: List a -> [a]
unList Nil = []
unList (x `Cons` xs) = x : unList (xs)

-- 1.в.
listEmpty :: List a -> Bool
listEmpty Nil = True
listEmpty _ = False

-- 1.г.
listHead :: List a -> a
listHead (x `Cons` xs) = x

-- 1.д.
listTail :: List a -> List a
listTail (Cons _ xs) = xs

-- 1.e.
listMap :: (a -> a) -> List a -> List a
listMap f Nil = Nil
listMap f (x `Cons` xs) = (f x) `Cons` (listMap f xs)

-- 1.ж.
listFilter :: (a -> Bool) -> List a -> List a
listFilter _ Nil = Nil
listFilter f (x `Cons` xs) = if (f x) then x `Cons` (listFilter f xs) else listFilter f xs

{- Двоични дървета -}
{-
Задача 2. Нека е дадено двоично дърво tree. Дефинирайте следните функции:

а). treeDepth tree, която връща дълбочината на дървото.
б). treeCountLeaves tree, която връща броя на листата на дървото.
в). treeSum tree, която връща сбора на всички стойности в дървото.
г). treeElem val tree, която проверява дали дадена стойност val е в дървото.
д). treeNodes tree, която връща списък със стойностите в дървото.
е). treeNodesAtLevel tree n, която връща списък със стойностите в n-тото ниво на дървото.
-}
treeDepth :: (Num b, Ord b) => Tree a -> b
treeDepth Empty = 0
treeDepth (Node value (tr1) (tr2)) = 1 + max (treeDepth tr1) (treeDepth tr2)

treeCountLeaves :: (Num b) => Tree a -> b
treeCountLeaves Empty = 0
treeCountLeaves (Node value (Empty) (Empty)) = 1
treeCountLeaves (Node value (tr1) (tr2)) = treeCountLeaves tr1 + treeCountLeaves tr2

treeSum :: Num a => Tree a -> a
treeSum Empty = 0
treeSum (Node value (tr1) (tr2)) = value + treeSum tr1 + treeSum tr2

treeElem :: Eq a => a -> Tree a -> Bool
treeElem _ Empty = False
treeElem x (Node value tr1 tr2) = (x == value) || treeElem x tr1 || treeElem x tr2

treeNodes :: Tree a -> [a]
treeNodes Empty = []
treeNodes (Node value tr1 tr2) = (treeNodes tr1) ++ [value] ++ (treeNodes tr2)

treeNodesAtLevel :: (Eq b, Num b) => Tree a -> b -> [a]
treeNodesAtLevel Empty _ = []
treeNodesAtLevel (Node value _ _) 0 = [value]
treeNodesAtLevel (Node _ tr1 tr2) n = (treeNodesAtLevel tr1 (n-1)) ++  (treeNodesAtLevel tr2 (n-1))

-- примерни извиквания --
main :: IO()
main = 
  let 
    {- 
    В примерите ще използваме следното дърво (tree): 

                                  3
                                /   \
                               1     4
                              / \   / \
                             0   2     5
                            / \ / \   / \
                                         6
    -}
    tree = (Node 3 (Node 1 (Node 0 Empty Empty) (Node 2 Empty Empty)) (Node 4 Empty (Node 5 Empty (Node 6 Empty Empty))))
  in do
    -- Пример 1.
    print $ distance (Point2D 1 1) (Point2D 2 2)

    -- Пример 2.
    print $ listAppend (1 `Cons` (2 `Cons` (3 `Cons` Nil))) 10
    print $ listAppend (1 `Cons` (2 `Cons` (3 `Cons` Nil))) 42

    -- Пример 3.
    print $ treeEmpty Empty
    print $ treeEmpty tree
    
    -- Пример 4.
    -- print $ treeRoot Empty
    print $ treeRoot tree
    
    -- Пример 5.
    print $ treeCount Empty
    print $ treeCount tree

    -- Задача 1.
    print $ mkList [1, 2, 3]
    print $ unList (1 `Cons` (2 `Cons` (3 `Cons` Nil)))
    print $ listEmpty (1 `Cons` (2 `Cons` (3 `Cons` Nil)))
    print $ listHead (1 `Cons` (2 `Cons` (3 `Cons` Nil)))
    print $ listTail (1 `Cons` (2 `Cons` (3 `Cons` Nil))) 
    print $ listMap (+1) (1 `Cons` (2 `Cons` (3 `Cons` Nil)))
    print $ listFilter even (1 `Cons` (2 `Cons` (3 `Cons` Nil)))

    -- Задача 2.
    print $ treeDepth Empty
    print $ treeDepth tree
    print $ treeCountLeaves Empty
    print $ treeCountLeaves tree
    print $ treeSum Empty
    print $ treeSum tree
    print $ 2 `treeElem` Empty
    print $ 2 `treeElem` tree
    print $ 7 `treeElem` tree
    print $ treeNodes tree
    print $ tree `treeNodesAtLevel` 0
    print $ tree `treeNodesAtLevel` 1
    print $ tree `treeNodesAtLevel` 2
    print $ tree `treeNodesAtLevel` 3
    print $ tree `treeNodesAtLevel` 4