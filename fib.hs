fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

--fib n = [fibonacci n | n <- [0..(n-1)]]
--fib n
-- | n == 0 = []
-- | n < 10 = (fibonacci n) : fib (n-1)

{-fib2 n
 | n == 0 = []
 | n < 10 = (fibonacci n) : fib2 (n-1)
-}

fib n
 | n == 0 = [fibonacci n]
 | otherwise = fib (n-1) ++ [fibonacci n]

range a b
 | a == b = []
 | a > b = a : range (a-1) b
 | a < b = a : range (a+1) b
