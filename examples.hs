module Examples where

-- (define add (lambda (x y) ( + x y))

add = \x y -> x + y -- curried version; Integer -> (Integer -> Integer)
addp = \(x,y) -> x + y

-- Homework:
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

member n [] = False
member n (x:xs)
  | n == x = True
  | otherwise = member n xs

f0 x = x + 1 -- type: int -> int
f1 = \x -> x + 1
f2 = \() -> 42 -- constant


