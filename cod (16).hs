----------Revisão P1-----------------

-------------------------------------
------Comandos ghci
-------------------------------------
{-
ghci    :l     :r
:q      :t     :h
-}
-------------------------------------
------Funções haskell
-------------------------------------
{-
sqrt = raiz quadrada
head
tail
take = pega n primeiros
drop = tira "    "
length
sum 
product
++
reverse
init = tira ultimo elem
even = par
odd = ímpar
not
and
zip ::[a]->[b]->[(a,b)]=1ºa com 1ºb ...
abs = módulo
&&
signum
concat 
-}
-------------------------------------
------Funcoes criadas  
-------------------------------------
{-
fatorial
media
second xs = head (tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
double x = x*2
palindrome xs = reverse xs == xs
twice f x = f (f x)
-}
-------------------------------------
------Tipos do haskell
-------------------------------------
{-
Bool = False | True
Char - Caracteres únicos
String - Sequências de caracteres
Int - Inteiros de precisão fixa
Integer - Inteiros de precisão arbitrária
Float - Número em ponto-flutuante
a - função polimórfica
-}
-------------------------------------
------Classes de tipo
-------------------------------------
{-
Num – Tipos Numéricos
Eq – Tipos Comparáveis
Ord – Tipos Ordenáveis
Exemplos:
(+) :: Num a => a -> a -> a
(==) :: Eq a => a -> a -> Bool
(<) :: Ord a => a -> a -> Bool
-}
-------------------------------------
-------Expressões condicionais
-------------------------------------
{-
if...then...
else if... then...
else...

funcao x | ... = ...      (guardas)
	     |otherwise = ...
		 
funcao n = m (casamento de padroes)
funcao m = n 
-}
-------------------------------------
-------Listas
-------------------------------------
{-
[1,2,3,4]
Significa 1:(2:(3:(4:[])))
-}
-------------------------------------
-------Expressões Lambda
-------------------------------------
{-
Expressões Lambda podem ser usadas para dar sentido formal às funções definidas através de currificação (currying). Por exemplo:
	add x y = x + y
Significa:
	add = \ x -> ( \ y -> x + y)
-}
-------------------------------------
-------Códigos
-------------------------------------

--Ordena vetor-----------------------
f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
         where
         ys = [ a | a <- xs, a <= x]
         zs = [ b | b <- xs, b > x]
--quicksort---------------------------
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
       where
           smaller = [a | a <- xs, a <= x]
           larger = [b | b <- xs, b > x]
--divisores---------------------------
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
--primo-------------------------------
prime :: Int -> Bool
prime n = factors n == [1,n]
--primos------------------------------ 
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]
--posicoes---------------------------
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']
-------------------------------------
-------novos tipos
-------------------------------------
{-
type = n pode ser recursivo
data = recursivo
-}



