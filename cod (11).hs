-- Declaring Types and Classes 
----------------------------------
--type
--para tipos existentes
type String = [Char] 

-- ou para melhor ler 
type Pos = (Int,Int) 

type Pair a = (a,a) 

mult :: Pair Int -> Int
mult (m,n) = m*n

copy :: a -> Pair a
copy x = (x,x) 

--um type n pode ser recursivo
type Trans = Pos -> Pos 
----------------------------------
--data
--definir novos tipos
--iniciam com letra maiuscula
--data Bool = False | True  -- f e t = construtores

data Answer = Yes | No | Unknown 

answers::[Answer]
answers = [Yes,No,Unknown]

flip :: Answer->Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown 

data Shape = Circle Float
 | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

--Circle :: Float -> Shape
--Rect :: Float -> Float -> Shape 
----------------------------------
data Maybe' a = Nothing' | Just' a

safediv :: Int -> Int -> Maybe' Int
safediv _ 0 = Nothing'
safediv m n = Just' (m `div` n)

safehead :: [a] -> Maybe' a
safehead [] = Nothing'
safehead xs = Just' (head xs) 
--------------------------------
data Nat = Zero | Succ Nat 

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add' :: Nat -> Nat -> Nat
add' m n = int2nat (nat2int m + nat2int n) 

add Zero n = n
add (Succ m) n = Succ (add m n) 

-- exe 1

mult' n Zero = Zero
mult' m (Succ n) = add m (mult' m n)
--------------------------------
data Expr = Val Int
 | Add Expr Expr
 | Mul Expr Expr

ex1=Add (Val 1) (Mul (Val 2) (Val 3))

size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y 

--2

-----------------------------------
data Tree a = Leaf a | Node (Tree a) a (Tree a) 

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9)) 

occurs x (Leaf y) = x == y
occurs x (Node l y r) | x == y = True
                      | x < y = occurs x l
                      | x > y = occurs x r 
--3
complete::Tree Int -> Bool
complete x | 
---------------------------------


