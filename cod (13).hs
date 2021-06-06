
-- prelude

halve::[a] -> ([a],[a])
halve xs = splitAt ((length xs) `div` 2) xs
{-
halve [] = ([],[])
halve xs = if (length xs) `mod` 2 == 0 then
               splitAt ((length xs) `div` 2) xs
           else error "lista de tamanho impar"
-}
-- ----------------------------------------------

third::[a] -> a
third xs = head (tail (tail xs))
-- if (lenght xs) >= 3 then
-- head (tail (tail xs)) 
-- else error "lista de tamanho < 3"

-- ----------------------------------------------

luhnDouble::Int->Int
luhnDouble x = if x*2 > 9 then x*2-9
               else x*2

luhn::Int->Int->Int->Int->Bool
luhn a b c d = sum [luhnDouble (a*2), luhnDouble b, luhnDouble (c*2), luhnDouble d] `mod` 10 == 0














