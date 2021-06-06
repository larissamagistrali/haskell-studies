map1 :: (a -> b) -> [a] -> [b] 
map1 f [] = []
map1 f (x:xs) = f x : map1 f xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p [] = []
filter1 p (x:xs)
 | p x = x : filter1 p xs
 | otherwise = filter1 p xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p xs = [x | x <- xs, p x] 

foldr1 :: (a → b → b) → b → [a] → b
foldr1 f v [] = v
foldr1 f v (x:xs) = f x (foldr1 f v xs)

--exercícios
--1
--2
--3
--map2 :: (a -> b) -> [a] -> [b] 
--filter3 :: (a -> Bool) -> [a] -> [a]
