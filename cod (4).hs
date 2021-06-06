-- exercício safetail
safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

safetailB:: [a] -> [a]
safetailB xs | null xs   = []
             | otherwise = tail xs

safetailC :: [a] -> [a]
safetailC []     = []
safetailC (x:xs) = xs

-- exercicio a
or' False False = False
or' False True  = True
or' True  False = True
or' True  True  = True

or'' False False = False
or'' _     _     = True
 
or''' False  b   = b
or''' True   _   = True

or'''' a b  | a == b = a
or'''' _ _           = True

-- exercicio d
and' :: Bool -> Bool -> Bool
and' a b = if a && b
        then True
        else False

-- exercício e
mult' :: Int -> (Int -> (Int -> Int))
mult' = \x -> (\y -> (\z -> x * y * z))
