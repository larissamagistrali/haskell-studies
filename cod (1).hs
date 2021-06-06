-- sintaxe
n = a `div` length xs
    where
        a  = 10
        xs = [1,2,3,4,5]

-- a faça duas versões da função last
last' xs = head (reverse xs)
last'' xs = xs !! (length xs - 1)

-- b faça duas versões de init -- obs: init remove apenas o último elemento
init' xs = take (length xs - 1) xs
init'' xs = reverse (tail (reverse xs))
