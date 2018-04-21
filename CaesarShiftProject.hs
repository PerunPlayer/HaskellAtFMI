import Data.Char
import Data.List
import Data.Maybe

main :: IO()
main = do
 print (normalizeMessage "Attack London tomorrow at ten a.m.")
 --print (normalizeMessage "Attack London tomorrow at 10 a.m.")--throw exception
 print (encode ['A'..'Z'] 'A' 5)
 print (encode ['A'..'Z'] 'S' 27)
 print (encode ['A'..'Z'] 'Z' (-3))
 print (encode ['A'..'Z'] 'A' (-5))
 --print (encode ['A'..'Z'] '@' 1)--throw exception
 print (encrypt ['A'..'Z'] 5 "ATTACKLONDONTOMORROWATTENAM")
 print (decrypt ['A'..'Z'] 5 "FYYFHPQTSITSYTRTWWTBFYYJSFR")
 print (crackall ['A'..'Z'] "FYYFHPQTSITSYTRTWWTBFYYJSFR")
 print (substring "Haskell" "Haskell Curry")
 print (substring "Turing" "Haskell Curry")
 print (crackcandidates ['A'..'Z'] ["THE","AND","AT","ON","IS"] "FYYFHPQTSITSYTRTWWTBFYYJSFR")
 print (polyencrypt ['A'..'Z'] 5 1 7 "ATTACKLONDONTOMORROWATTENAM")
 print (polydecrypt ['A'..'Z'] 5 1 7 "FYYFHPQUTJUTZUTVYYVDHBBMVIU")
 
{-
Напишете функция normalize message​, която нормализира входното
съобщение. Правилата за нормализация са следните:
- Всички малки букви стават главни.
- Ако съобщението съдържа цифри, функцията връща грешка.
- Всички останали знакове се игнорират.
Примери:
normalize "Attack London tomorrow at ten a.m." = "ATTACKLONDONTOMORROWATTENAM"
normalize "Attack London tomorrow at 10 a.m." = error “digits not allowed”
-}

normalizeMessage :: String -> String
normalizeMessage "" = ""
normalizeMessage (c:cs)
 | (c >= '1' && c <= '9') = error "digits not allowed"
 | (c >= 'a' && c <= 'z') = (toUpper c) : normalizeMessage cs
 | (c >= 'A' && c <= 'Z') = c : normalizeMessage cs
 | otherwise = normalizeMessage cs
 
 
{-
Напишете функция encode alphabet ch offset​, която приема списък от знакове
alphabet, знак ch и отместване offset и връща знака от alphabet, отместен на offset от ch
(по модул дължината на списъкa). Функцията encode трябва да работи както с
положително, така и с отрицателно отместване и да връща грешка, ако ch не е
елемент на alphabet
-}

encode :: [Char] -> Char -> Int -> Char
encode alphabet ch offset 
 | not (elem ch alphabet) = error "unsupported symbol"
 | otherwise = (alphabet !! findChar)
  where
   findChar
    | ((offset + (fromJust (elemIndex ch alphabet))) > length alphabet) = (offset + (fromJust (elemIndex ch alphabet)) - (length alphabet))
    | ((offset + (fromJust (elemIndex ch alphabet))) < 0) = (offset + (fromJust (elemIndex ch alphabet)) + (length alphabet))
    | otherwise = (offset + (fromJust (elemIndex ch alphabet)))
{-
Напишете функция encrypt alphabet offset normalized​, която приема азбука
alphabet, отместване offset и съобщение в нормализиран вид и връща съобщението,
криптирано със съответното отместване.
Пример:
encrypt ['A'..'Z'] 5 "ATTACKLONDONTOMORROWATTENAM" = "FYYFHPQTSITSYTRTWWTBFYYJSFR"
-}

encrypt :: [Char] -> Int -> [Char] -> [Char]
encrypt alphabet offset "" = []
encrypt alphabet offset normalized = (encode alphabet (head normalized) offset) : (encrypt alphabet offset (tail normalized))

{-
Напишете функция decrypt alphabet offset ecrypted​, която приема отместване
offset и съобщение, криптирано с това отместване, и връща оригиналното съобщение
в нормализиран вид. Можете да използвате факта, че декриптирането на Цезаров
шифър с отместване offset е еквивалентно на криптиране с отместване -offset.
Пример:
decrypt ['A'..'Z'] 5 "FYYFHPQTSITSYTRTWWTBFYYJSFR" = "ATTACKLONDONTOMORROWATTENAM"
-}
decrypt :: [Char] -> Int -> [Char] -> [Char]
decrypt alphabet offset "" = []
decrypt alphabet offset encrypted = (encode alphabet (head encrypted) (-offset)) : (decrypt alphabet offset (tail encrypted))

{-
Напишете функцията crackall alphabet encrypted​, която връща списък от
всички възможни дешифровки на кодираното съобщение enccrypted.
-}

crackall :: [Char] -> [Char] -> [[Char]]
crackall alphabet encrypted = helper ((length alphabet) - 1)
 where 
  helper :: Int -> [[Char]]
  helper count 
   | count == 0 = []
   | otherwise = (decrypt alphabet count encrypted) : (helper (count - 1))

{-
Напишете функция substring sub str​, която проверява дали поднизът sub се среща в низа str.
-}

--isInfixOf
substring :: (Eq a) => [a] -> [a] -> Bool
substring sub str = any (isPrefixOf sub) (tails str)

{-
Напишете функцията crackcandidates alphabet commonwords encrypted​, която приема списък с често
срещани думи и криптирано съобщение и връща списък с потенициални вероятни разшифровки.
Една разшифровка се смята за вероятна, ако съдържа поне една от думите от списъка с често срещани думи.
Пример:
crackcandidates ['A'..'Z'] ["THE","AND","AT","ON","IS"]
"FYYFHPQTSITSYTRTWWTBFYYJSFR" = ["ATTACKLONDONTOMORROWATTENAM"]
-}

crackcandidates :: [Char] -> [[Char]] -> [Char] -> [[Char]]
crackcandidates alphabet commonwords encrypted = filter (\x -> containsAtLeastOne ((length commonwords) - 1) x) possibledecrypted
 where
  possibledecrypted = (crackall alphabet encrypted)
  containsAtLeastOne :: Int -> [Char] -> Bool
  containsAtLeastOne count message
   | count == -1 = False
   | substring (commonwords !! (count)) message = True
   | otherwise = (False || containsAtLeastOne (count - 1) message)
   
{-
Напишете функция​, която приема азбука alphabet, първоначално отместване offset, стъпка step и размер
на блока blockSize, както и съобщение в нормализиран вид, и връща криптирано
съобщение, първите blockSize знака на което са криптират с отместване offset,
следващите blockSize знака - с отместване offset + step, и т. н.
Пример:
polyencrypt ['A'..'Z'] 5 1 7 "ATTACKLONDONTOMORROWATTENAM" =
"FYYFHPQUTJUTZUTVYYVDHBBMVIU"
-}

polyencrypt :: [Char] -> Int -> Int -> Int -> [Char] -> [Char]
polyencrypt alphabet offset step blockSize [] = []
polyencrypt alphabet offset step blockSize normalized = (encrypt alphabet offset curr) ++ (polyencrypt alphabet (offset + step) step blockSize remaining)
 where
  curr = fst (splitAt blockSize normalized)
  remaining = snd (splitAt blockSize normalized)
  
{-
Напишете функция polydecrypt alphabet offset step blockSize encrypted​,
която декриптира съобщението от предишната подточка.
Пример:
polydecrypt ['A'..'Z'] 5 1 7 "FYYFHPQUTJUTZUTVYYVDHBBMVIU" =
"ATTACKLONDONTOMORROWATTENAM"
-}

polydecrypt :: [Char] -> Int -> Int -> Int -> [Char] -> [Char]
polydecrypt alphabet offset step blockSize [] = []
polydecrypt alphabet offset step blockSize encrypted = (decrypt alphabet offset curr) ++ (polydecrypt alphabet (offset + step) step blockSize remaining)
 where
  curr = fst (splitAt blockSize encrypted)
  remaining = snd (splitAt blockSize encrypted)