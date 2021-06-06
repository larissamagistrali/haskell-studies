qsort :: Ord a => [a] -> [a]
qsort []= []
qsort (x:xs)= qsort smaller ++ [x] ++ qsort larger
 where
  smaller = [a | a <- xs, a <= x]
  larger  = [b | b <- xs, b > x]

{-teste-}

-- exerc a
soma :: Num a => [a] -> a
soma [] = 0
soma (x:xs) = x + soma xs

-- exerc b
produtorio :: [Int] -> Int
produtorio [] = 1
produtorio (x:xs) = x * produtorio xs

-- exerc c, decrescente
qsortD :: Ord a => [a] -> [a]
qsortD []= []
qsortD (x:xs)= qsortD smaller ++ [x] ++ qsortD larger
 where
  smaller = [a | a <- xs, a >= x]
  larger  = [b | b <- xs, b < x]

-- exerc d, ignora numeros iguais
qsort2 :: Ord a => [a] -> [a]
qsort2 []= []
qsort2 (x:xs)= qsort2 smaller ++ [x] ++ qsort2 larger
 where
  smaller = [a | a <- xs, a < x]
  larger  = [b | b <- xs, b > x]

-- exerc e
{-
qsort :: Ord a => [a] -> [a]
qsort []= []
qsort (x:xs)= qsort smaller ++ [x] ++ qsort larger
 where
  smaller = [a | a <- xs, a <= x]
  larger  = [b | b <- xs, b > x]

qsort [3,5,1,4,2]

-}

-- exerc f
--"345adf"

-- exerc g
--2^3*4	  = (2^3)*4
--2*3+4*5 = (2*3)+(4*5)
--2+3*4^5 = 2+(3*(4^5))

-- exerc h
{-
Não
*Main> head []
*** Exception: Prelude.head: empty list
-}

-- exerc i
{-
<interactive>:40:7: error:Variable not in scope: double :: Integer -> Int
take :: Int -> [a] -> [a]
-}

-- exerc j
n = a `div` length xs
 where
  a = 10
  xs = [1,2,3,4,5]
--ou 
n2 = div a (length xs)
 where
  a = 10
  xs = [1,2,3,4,5]

