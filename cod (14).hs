-- Countdown example from chapter 9 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

-- Arithmetic operators

data Op = Add | Sub | Mul | Div

instance Show Op where
   show Add = "+"
   show Sub = "-"
   show Mul = "*"
   show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Numeric expressions

data Expr = Val Int | App Op Expr Expr

-- show (App Add (App Add (Val 1) (Val 2)) (Val 1))
-- "(1+2)+1"

--show (App Sub (Val 0) (Val 1))
--"0-1"

-- 3+(7*20)
--(App Add (Val 3) (App Mul (Val 7) (Val 20)))

instance Show Expr where 
   show (Val n)     = show n
   show (App o l r) = brak l ++ show o ++ brak r
                      where
                         brak (Val n) = show n
                         brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

-- Combinatorial functions

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs
{- 
[2,3]
yss ++ map (2:) yss
(subs [3] ++ map (2:) (subs [3]))
((yss ++ map (3:) yss) ++ map (2:) (yss ++ map (3:) yss))
(([] ++ map (3:) []) ++ map (2:) ([] ++ map (3:) []))
([] ++ [3]) ++ map (2:) ([] ++ [3])
[[],[3]] ++ map (2:) [[],[3]]
[[],[3]] ++ [[2],[2,3]]
[[],[3],[2],[2,3]]
-}

{-
EXERCÍCIO SUBS: [1,2,3]
RESULTADO: [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
PASSO A PASSO: 
***FAZER***
(subs [2,3] ++ map (1:) subs [2,3])
((subs [3] ++ map (2:) subs [3]) ++map(1:)(subs [3] ++ map (2:) subs [3]))

-}

interleave :: a -> [a] -> [[a]] -- intercala elem x na lista
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

{-
interleave 2 [3,5]
R: [[2,3,5],[3,2,5],[3,5,2]]
PASSO A PASSO: 
[2:3:5]:map (3:) (interleave 2 [5])
[2:3:5]:map (3:) ([2:5:[]]:map [5] (interleave 2 []))
[2:3:5]:map (3:) ([2:5:[]]:map [5] ([[2]]))
[2,3,5]:map (3:) ([2,5]: map [5,2])
***FAZER***
-}

perms :: [a] -> [[a]] -- todas as permutações da lista
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]] -- conjunto potencia
choices = concat . map perms . subs 

------------------Exercicio------------------------
--Refazer choices sem usar map,composiçoes e concat. 
--Usar perms e subs
choices2::[a]->[[a]]
choices2 xs = [zs|ys<-subs xs,zs<-perms ys] 

--Decide se uma lista 's' esta em choices de 'u'. Tanto s
--quanto u são listas d mesmo tipo. Ñ use perms e subs.
isChoice::Eq a=>[a]->[a]->Bool
isChoice xs ys = or [a==xs|a<-(choices ys)]

--Recursivo
isChoice2 :: Eq a => [a] -> [a] -> Bool
isChoice2 [] _      = True
isChoice2 [_] []    = False
isChoice2 (x:xs) ys = elem x ys && isChoice2 xs (remove x ys)

remove :: Eq a => a -> [a] -> [a]
remove x [] = error "Target not found"
remove x (y:ys) | x == y    = ys
                | otherwise = y : remove x ys

----------------------------------------------------

-- Formalising the problem

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- Brute force solution

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                 l       <- exprs ls,
                 r       <- exprs rs,
                 e       <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- Combining generation and evaluation

type Result = (Expr,Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n > 0]
results ns  = [res | (ls,rs) <- split ns,
                      lx     <- results ls,
                      ry     <- results rs,
                      res    <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

-- Exploiting algebraic properties

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

results' :: [Int] -> [Result]
results' []  = []
results' [n] = [(Val n,n) | n > 0]
results' ns  = [res | (ls,rs) <- split ns,
                       lx     <- results' ls,
                       ry     <- results' rs,
                       res    <- combine'' lx ry]

combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | ns' <- choices ns, (e,m) <- results' ns', m == n]

-- Performance testing

main :: IO ()
main = print (solutions'' [1,3,7,10,25,50] 765)
