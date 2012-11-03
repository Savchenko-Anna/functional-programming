{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil | Cons a (List a) deriving (Show,Read)

---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil = Zero
length (Cons a as) = Succ (length as)

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil ++ a = a
a ++ Nil = a
(Cons a as) ++ b = Cons a (as ++ b)

-- Список без первого элемента
tail :: List a -> List a
tail Nil = Nil
tail (Cons a as) = as

-- Список без последнего элемента
init :: List a -> List a
init Nil = Nil
init (Cons a Nil) = Nil
init (Cons a as) = Cons a (init as)

-- Первый элемент
head :: List a -> a
head Nil = error "the list is empty"
head (Cons a as) = a

-- Последний элемент
last :: List a -> a
last Nil = error "the list is empty"
last (Cons a Nil) = a
last (Cons a as) = last as

-- n первых элементов списка
take :: Nat -> List a -> List a
take n Nil = Nil
take Zero a = Nil
take (Succ n) (Cons a as) = Cons a (take n as)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop n Nil = Nil
drop Zero a = a
drop (Succ n) a = drop n (tail a)

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a as) = if' (p a) (Cons a (filter p as)) (filter p as)

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
--data Maybe a = Nothing | Just a deriving (Show,Read)
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p Nil = Nil
gfilter p (Cons a as) = case p a of
  Just b -> Cons b (gfilter p as)
  Nothing -> gfilter p as

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takewhile p Nil = Nil
takeWhile p (Cons a as) = if' (p a) (Cons a (takeWhile p as)) Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil = Nil
dropWhile p (Cons a as) = if' (p a) (dropWhile p as) (Cons a as) 


-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p Nil = Pair Nil Nil
span p (Cons a as) = if' (p a) (Pair (Cons a b) c) (Pair Nil (Cons a as)) where (Pair b c) = span p as

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p a = span (not . p) a

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
a !! Zero = head a
a !! (Succ n) = (tail a) !! n

-- Список задом на перёд
reverse :: List a -> List a
reverse Nil = Nil
reverse (Cons a Nil) = Cons a Nil
reverse (Cons a as) = (reverse as) ++ (Cons a Nil)

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) = undefined

-- (*) Все перестановки элементов данного списка
permutations :: List a -> List (List a)
permutations = undefined

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat a = Cons a (repeat a)

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--        f ...
--       / \
--      f   l!!2
--     / \
--    f   l!!1
--   / \
--  z   l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z Nil = z
foldl f z (Cons a as) = foldl f (f z a) as

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z Nil = Cons z Nil
scanl f z (Cons a as) = Cons (f z a) (scanl f (f z a) as)

-- Правая свёртка
-- порождает такое дерево вычислений:
-- f
-- / \
-- l!!0 f
-- / \
-- l!!1 f
-- / \
-- l!!2 ...
-- \
-- z
-- 
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z Nil = z
foldr f z (Cons a as) = f a (foldr f z as)

-- Аналогично
-- head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f z Nil = Cons z Nil
scanr f z (Cons a as) = Cons (f a r) rs where (Cons r rs) = scanr f z as

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons a as) = Cons (f a) (map f as)

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons a as) = a ++ (concat as)

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap f Nil = Nil
concatMap f (Cons a as) = (f a) ++ (concatMap f as)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip a Nil = Nil
zip Nil b = Nil
zip (Cons a as) (Cons b bs) = Cons (Pair a b) (zip as bs)

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f a Nil = Nil
zipWith f Nil b = Nil
zipWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith f as bs)