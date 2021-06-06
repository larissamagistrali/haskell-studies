--PARTE-1
--ex1
--4104 => 2^3+16^3 = 9^3+15^3
quadruplas::Int->[(Int,Int,Int,Int)]
quadruplas n = [(a,b,c,d)|a<-[1..n],b<-[a..n],c<-[a+1..n],d<-[c..n],
                         (a^3)+(b^3)==(c^3)+(d^3),
                          a<=b,c<=d,a<c]
--ex2
disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint xs ys = not (or [x==y|x<-xs,y<-ys])

--ex3
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

tBal :: Tree Int
tBal = Node  (Node (Leaf 1) (Leaf 4)) 
                (Node (Leaf 6) (Leaf 9)) 

balance     :: [a] -> Tree a
balance []  =  error "lista vazia!"
balance [x] =  Leaf x
balance xs  =  Node (balance (fst (halveList xs))) (balance (snd (halveList xs)))  

halveList :: [a] -> ([a],[a])
halveList [] = ([],[])
halveList xs = splitAt (half xs) xs
             where
                  half :: [a] -> Int
                  half xs = div (length xs) 2 
--PARTE-2
--ex1
{-
fst(1+2, 2+3)
Outmost eh melhor pq faz menos operações.

outmost
fst(1+2, 2+3)
1+2
3

innermost
fst(1+2, 2+3)
fst(3,2+3)
fst(3,5)
3

-}

--ex2
armstrong::Int->Bool
armstrong n = sum [x^(length (digits n))|x<-(digits n)] == n

digits :: Int -> [Int]
digits = map (read . (:[])) . show

--ex3
fibs::[Integer]
fibs = [0,1] ++ [x+y|(x,y)<-zip fibs (tail fibs)]





















