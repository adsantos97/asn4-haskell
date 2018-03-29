fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

{-fib n
 | n == 10 = []
 | n == 0 = [n]
 | n == 1 = [n]
 | otherwise = n:(fib (n-1) + fib (n-2))
-}

{-fib n
 | n == 0 = 0:[]
 | n == 1 = 0:1:[]
 | n == 2 = 0:1:1:[]
 | n == 3 = 0:1:1:2:[]
 | otherwise = [0, 1, 1, 2, 3]
-}

fib n = [fibonacci n | n <- [1..n]]

--repeat' x = x:repeat' x 
