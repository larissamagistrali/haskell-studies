import System.IO 
--putChar,getChar,return

act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

getLine' :: IO String
getLine' = do x <- getChar
              if x=='\n' then 
                 return []
              else 
                 do xs <- getLine'
                    return (x:xs) 

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs 

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n' 

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"
-------------------------------------------------
hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word 

sgetLine :: IO String
sgetLine = do x <- getCh -- do = sequencia de operacoes
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs) 

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> IO ()
play word =
     do putStr "? "
        guess <- getLine
        if guess == word then
           putStrLn "You got it!"
        else
           do putStrLn (match word guess)
              play word 

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

-----------------------------------------------------
{-
Exercício

f1: * * * * *
f2: * * * *
f3: * * *
f4: * *
f5: * 
-}

f1=["*","*","*","*","*"]
f2=["*","*","*","*"]
f3=["*","*","*"]
f4=["*","*"]
f5=["*"]

--playNim::

playNim::IO ()
playNim =
        do putStr ((show f1) ++ "\n" ++ (show f2) ++ "\n" ++ (show f3) ++ "\n" ++ (show f4) ++ "\n" ++ (show f5) ++ "\n")
           putStr "Fila: "
           fila<-getLine
           if fila=="f1" || fila =="f2" || fila =="f3" || fila =="f4" || fila =="f5" then
              do putStr "Qtd:"
                 qtd<-getLine
              
           else
              do putStr "fila incorreta!"           










