{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read,error)

---------------------------------------------
-- ��������� ������-���������

-- ������������� �����������
example1 x = x
example1' = \x -> x
example1'' = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- ����� ������������� �����������
example2 x y = x %+ y
example2' x = \y -> x %+ y
example2'' = \x -> \y -> x %+ y
example2''' = \x y -> x %+ y
example2'''' = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- ����������� ���������
undefined = undefined

-- ���� ������� ����������� ��� �����, ��������� �� undefined ��������.
-- ����� ����� ����� ������������ (natEq � natLt --- ������� ���������).

-------------------------------------------
-- ����������� ����

-- ��� � ������������ ���������
data Unit = Unit deriving (Show,Read)

-- ����, ������������
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- �������, ��������������
data Either a b = Left a | Right b deriving (Show,Read)

-- ������ ������� ������, ��������� Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- ������ ������� ������, ��������� Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- ������� ��������, ��� ���������� if � ���� Bool ������������ ������,
-- ���� case ������ ��������.

-- �� ��� ����� ����������� ���� if
if' True a b = a
if' False a b = b

-- ����������. ������������� ���, ������������ ��������� ���������
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- ������ ��������

-- ���������� "��"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- ���������� "�"
(&&) :: Bool -> Bool -> Bool
True && x = x
False && _ = False

infixr 2 ||
-- ���������� "���"
(||) :: Bool -> Bool -> Bool
True || _ = True
False || x = x

-------------------------------------------
-- ����������� �����

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero -- 0
natOne = Succ Zero -- 1

-- ���������� ��� ����������� �����
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero (Succ x) = LT
natCmp (Succ x) Zero = GT
natCmp (Succ x) (Succ y) = natCmp x y

-- n ��������� � m
natEq :: Nat -> Nat -> Bool
natEq Zero Zero = True
natEq Zero (Succ _) = False
natEq (Succ _) Zero = False
natEq (Succ n) (Succ m) = natEq n m

-- n ������ m
natLt :: Nat -> Nat -> Bool
natLt Zero Zero = False
natLt Zero (Succ m) = True
natLt (Succ n) Zero = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- �������� ��� ����������� �����
(+.) :: Nat -> Nat -> Nat
Zero +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- ��������� ��� ����������� �����
(-.) :: Nat -> Nat -> Nat
n -. Zero = n
Zero -. m = Zero
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- ��������� ��� ����������� �����
(*.) :: Nat -> Nat -> Nat
Zero *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- ����� � ������� �� ������� n �� m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n Zero = error "division by zero"
natDivMod Zero n = Pair Zero Zero
natDivMod n m = if' (natLt n m) (Pair Zero n) (Pair (fst (natDivMod (n -. m) m) +. natOne) (snd (natDivMod (n -. m) m)))


natDiv n = fst . natDivMod n -- �����
natMod n = snd . natDivMod n -- �������

-- ����� GCD ���������� ������� (������ �������� 2 (���������������� �����) + 1 (���) �������)
gcd :: Nat -> Nat -> Nat
gcd a Zero = a
gcd a b = gcd b (natMod a b)  


-------------------------------------------
-- ����� �����

-- ���������, ����� ������������� ������� ����� ���� ������������
data Int = Pos Nat | Neg Nat deriving (Show,Read)

intZero = Pos Zero -- 0
intOne = Pos (Succ Zero) -- 1
intNegOne = Neg Zero -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos Zero) = Pos Zero
intNeg (Pos (Succ n)) = Neg n
intNeg (Neg n) = Pos (Succ n)

-- ������ ����� ��� ��� �����������
intCmp :: Int -> Int -> Tri
intCmp (Neg a) (Pos b) = LT
intCmp (Pos a) (Neg b) = GT
intCmp (Pos a) (Pos b) = natCmp a b
intCmp (Neg a) (Neg b) = natCmp b a

intEq :: Int -> Int -> Bool
intEq (Pos n) (Pos m) = (natEq n m)
intEq (Neg n) (Neg m) = (natEq n m)
intEq (Pos n) (Neg m) = False
intEq (Neg n) (Pos m) = False

intLt :: Int -> Int -> Bool
intLt (Pos n) (Pos m) = (natLt n m)
intLt (Neg n) (Neg m) = (natLt m n)
intLt (Neg n) (Pos m) = True
intLt (Pos n) (Neg m) = False

infixl 6 .+., .-.
-- � ���� ��� ������������ �������� ���� �� ��� �����
(.+.) :: Int -> Int -> Int
n .+. (Pos Zero) = n
(Pos n) .+. (Pos m) = Pos (n +. m)
(Neg n) .+. (Neg m) = Neg (Succ n +. m)
(Pos n) .+. (Neg m) = Pos n .+. Neg m
(Neg n) .+. (Pos m) = Pos m .+. Neg n
--(Pos (Succ n)) .+. (Neg Zero) = Pos n

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
a .*. (Pos Zero) = Pos Zero
(Pos a) .*. (Pos b) = Pos (a *. b)
(Neg a) .*. (Neg b) = Pos ((Succ a) *. (Succ b))
(Pos a) .*. (Neg b) = intNeg (Pos a .*. Pos (Succ b))
(Neg a) .*. (Pos b) = intNeg (Pos (Succ a) .*. Pos b)
-------------------------------------------
-- ������������ �����

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- � ������������ ��� ���� �������� ��������
ratInv :: Rat -> Rat
ratInv (Rat a Zero) = error "zero in denominator"
ratInv (Rat (Pos Zero) b) = error "can't put zero into denominator"
ratInv (Rat (Pos  a) b) = (Rat (Pos b) a)
ratInv (Rat (Neg a) (Succ b)) = Rat (Neg b) (Succ a)


-- ������ ��� ������
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat a Zero) (Rat c d) = error "zero in denominator"
ratCmp (Rat a b) (Rat c Zero) = error "zero in denominator"
ratCmp (Rat a b) (Rat c d) = intCmp (a .*. (Pos d)) (c .*. (Pos b))


ratEq :: Rat -> Rat -> Bool
ratEq (Rat a b) (Rat c d) = (intEq (a .*. (Pos d)) (c .*. (Pos b)))

ratLt :: Rat -> Rat -> Bool
ratLt (Rat a b) (Rat c d) = (intLt (a .*. (Pos d)) (c .*. (Pos b)))


infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat a Zero) %+ (Rat c d) = error "zero in denominator"
(Rat a b) %+ (Rat c Zero) = error "zero in denominator"
(Rat a  b) %+ (Rat c d) = Rat (a .*. (Pos d) .+. c .*. (Pos b)) (b *. d)

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat a Zero) %* (Rat c d) = error "zero in denominator"
(Rat a b) %* (Rat c Zero) = error "zero in denominator"
(Rat a b) %* (Rat c d) = Rat (a .*. c) (b *. d)

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- �������� ��� ���������.
-- ���������� �����, �� ������������ ����� � ����

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- ������������� �����������
example3 a b c = gcd a (gcd b c)
example3' a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- � ��� ������������� �����������
example4 a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b