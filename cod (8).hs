qsort :: Ord a=>[a]->[a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
       where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

-- Exercícios slides aula 9
-- copiar do caderno 
--3
{-
mergeSort :: ([a] -> [a] -> [a]) -> [a] -> [a]
mergeSort merge xs
    | length xs < 2 = xs
    | otherwise = merge (mergeSort merge first) (mergeSort merge second)
    where first = take half xs
        second = drop half xs 
        half = (length xs) `div` 2
-}
        
-- Exercícios livro pag 72
init1::[a]->[a]
init1 (x:xs) | null xs = []
            | otherwise = x:init1 xs 

init2::[a]->[a]
init2 [_] = []
init2 (x:xs) = x:init2 xs

--1 
fatorial::Int->Int
fatorial n |n==0=1
           |n>0 = n * fatorial (n-1)
           |otherwise = error "Num negativo!" 
--2
sumdown::Int->Int
sumdown n   |n==0=0
            |n>0 = n + sumdown (n-1)
            |otherwise = error "Num negativo!" 
--3
expo m n |m>=0 && n==0 = 1 -- m^n
         |m>=0 && n==1 = m
         |m>=0 && n>1  = m * expo m (n-1) 
         |otherwise = error "Num negativo!" 
--4
euclid::Int->Int->Int -- maximo divisor comum
euclid m n |m==n=m
           |m>n = euclid (m-n) n
           |otherwise = euclid (n-m) m

--5
{-
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

length[1,2,3]
1 +length[2,3]
1+1+length[3]
1+1+1+length[]
1+1+1+0
3
----------------------------
drop::Int->Int->Int
drop 0 xs = xs
dros _ [] = []
drop n (_:xs) = drop (n-1) xs

drop 3 [1,2,3,4,5]
drop 2 [2,3,4,5]
drop 1 [3,4,5]
drop 0 [4,5]
[4,5]
---------------------------
init :: [a] -> [a] -- tira ultimo elemento
init [_] = []
init (x:xs) = x : init xs

init [1,2,3]
1:init[2,3]
1:2:init[3]
1:2:[]
[1,2]
-}
--6
--6a
and'::[Bool]->Bool
and' [] = True
and' (x:xs) | x==False=False
            | otherwise = and' xs
--6b
concat'::[[a]]->[a]
concat' [] = []
concat' (x:xs)=x ++ concat' xs
--6c
replicate'::Int->a->[a]
replicate' 0 a = []
replicate' n a = [a] ++ replicate' (n-1) a 
--6d
nth :: [a] -> Int -> a
nth [] n = error "erro pos"
nth (x:xs) 1 = x
nth (x:xs) n = nth xs (n-1)
--6e
elem'::Eq a => a -> [a] -> Bool
elem' m [] = False
elem' m (x:xs) | m == x = True
               |otherwise = elem' m xs  
--7
--8













