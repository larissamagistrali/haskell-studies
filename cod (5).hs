--a
pyths::Int -> [(Int, Int, Int)]
pyths n = [(a,b,c) | a <- r, b <- r, c <- r, a^2 + b^2 == c^2] 
          where r = [1..n]
--b
fatores::Int->[Int]
fatores n = [x | x <- [1..n],n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (fatores x) - x == x]
--c
scalarProduct::[Int]->[Int]->Int
scalarProduct xs ys = sum[x*y | (x,y)<-zip xs ys] 
