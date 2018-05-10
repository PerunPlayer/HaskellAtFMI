main :: IO()
main = do
 print ((pairCompose [(\x -> x+1),(\x -> x+2),(\x -> x+3)]) 1)
 print ((switchsum (\x -> x + 1) (\x -> x * 2) 4) 2)
 print (replaceAssoc [5,4,2,3] [(1,5),(3,7),(5,9),(7,11),(9,13)])
 print (numOfNodes [(10,[3,7,12]),(3,[5,8,9]),(7,[11,13]),(12,[6,4]),(8,[1,2])])

{-
Да се дефинира функция, която получава като аргумент списък с функции от тип Int -> Int и връща
нова едноаргументна числова функция g – такава, че оценката на (g x) е равна на
сумата (f1.f2) (x) + (f3.f4) (x) + ... + (fn-1.fn) (x), където “.” е
операторът за композиция на функции. Ако оригиналният списък с функции има
нечетен брой елементи, то последната функция от списъка се композира с функцията идентитет (id).
-}

pairCompose :: [Int -> Int] -> (Int -> Int)
pairCompose [] x = 0
pairCompose (f:g:fs) x = compFirst2Funcs + pairCompose fs x
 where compFirst2Funcs = (f . g) x
pairCompose (f:fs) x = (f . (\x -> x)) x

{-
Ако f и g са числови функции и n е естествено число, да се дефинира
функция от по-висок ред, която връща като резултат функция,
чиято стойност в дадена точка x е равна на f(x)+g(f(x))+f(g(f(x)))+...
(сумата включва n събираеми).
-}

currFunc :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
currFunc f g 0 x = 0
currFunc f g 1 x = f x
currFunc f g n x = if even n then (g ((currFunc f g (n-1)) x)) else (f ((currFunc f g (n-1)) x))

switchsum :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
switchsum f g 0 x = 0
switchsum f g n x = (currFunc f g n x) + (switchsum f g (n-1) x) 

{-
Да се дефинира функция, която получава като аргументи списък list,
чийто елементи са цели числа, и речник – асоциативен списък dict,
чийто елементи са двойки от цели числа. Функцията трябва да върне нов
списък, в който всеки елемент се получава чрез замяна на съответния елемент на list
с асоциираната с него стойност в dict, ако в dict съществува елемент с такъв ключ,
или е равен на съответния елемент на list – в противен случай.
-}

type Pair = (Int, Int)
p :: Pair
p = (1, 5)

replaceAssoc :: [Int] -> [Pair] -> [Int]
replaceAssoc [] _ = []
replaceAssoc list [] = list
replaceAssoc (x:list) dict = repl : (replaceAssoc list dict)
 where repl = if null [n | (m, n) <- dict, m == x] then x else (head [n | (m, n) <- dict, m == x])
 
{-
Дадено е дърво tree от цели числа, представено с асоциативен списък,
описващ преките наследници (синовете) на върховете, които не са листа. Да се
дефинира функция, която намира броя на вътрешните върхове
node на tree, за които сумата на синовете на node е равна по стойност на родителя
на node.
-}

type Node = (Int, [Int])
n :: Node
n = (10,[3,7,12])

numOfNodes :: [Node] -> Int
numOfNodes [] = 0
numOfNodes tree = length [m | (m, n) <- tree, (sum n) == (parent m)]
 where parent m = if null [u | (u, v) <- tree, elem m v] then 0 else head [u | (u, v) <- tree, elem m v]