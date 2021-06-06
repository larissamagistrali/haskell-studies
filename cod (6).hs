--foldr funcao base lista
-- odd=ímpar, even=par
-- Exerc Livro Hutton, pag 89
--1
mapfilter::(a->a)->(a->Bool)->[a]->[a]
mapfilter f p xs = map f (filter p xs)

--2
--2a
all'::(a->Bool)->[a]->Bool
all' f xs = and [f x | x <- xs]

all''::(a->Bool)->[a]->Bool
all'' f xs = and (map f xs)

--2b
any'::(a->Bool)->[a]->Bool
any' f xs = or [f x | x <- xs]

any''::(a->Bool)->[a]->Bool
any'' f xs = or (map f xs)

--2c
takeWhile'::(a->Bool)->[a]->[a]
takeWhile' f [] = []
takeWhile' f (x:xs) 
     |f x = x:takeWhile' f xs
     |otherwise = []

--2d
dropWhile'::(a->Bool)->[a]->[a]
dropWhile' p [] = []
dropWhile' p (x:xs)
     | p x = dropWhile' p xs
     | otherwise = x:xs              

--3
map'::(a->b)->[a]->[b]
map' f = foldr (\x xs -> (f x):xs) []

filter'::(a->Bool)->[a]->[a]
filter' p (x:xs) = foldr (\x xs -> if p x then x:xs else xs) []

--4
dec2int::[Int]->Int
dec2Int xs = foldl (++) 0 xs 


--correção pag 86
rmdUps [] = []
rmdUps (x:xs) = x:rmdUps (filter (/=x) xs)



















