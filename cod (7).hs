{-

Redex == reducible expression
ordem das avaliações

avaliação mais interna(innermost) ($!)
avaliação mais externa(outmost)
---------------------------------
square::Int->Int
square n = n*n

innermost
square (1+2)
square (3)
3*3
9

outmost
square (1+2)
(1+2)*(1*2) //sharing
3*3
9
--------------------------------------
avaliação preguiçosa = outmost+sharing (otimização)

--------------------------------------
Ler Lambda expression - livro Hutton 15.2
Exercicios 1,2,3 - livro Hutton 15.9

---------------------------------------
Aula do dia 07/05 - livro Hutton 15.6

---------------------------------------
sumwith ... COPIAR FOTO
---------------------------------------

-}
sumwith::Int->[Int]->Int
sumwith v [] = v
sumwith v (x:xs) = sumwith (v+x) xs 

sumwith' v [] = v
sumwith' v (x:xs) = (sumwith $! (v+x)) xs

inf::Int
inf = 1+inf

fst'::(a,a)-> a
fst' (x,y) = x
