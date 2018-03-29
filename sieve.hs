{-
(define (mod a b) (remainder a b))

(define (sieve filter n)
  (cond ((> n 1000) nil)
    ((filter n) (sieve filter (+ n 1)))
    (else       (cons n (sieve (lambda (x) (if (equal? 0 (mod x n))
                                             #T
                                           (filter x)))
                        (+ n 1))))
  )
)

(define false (lambda (a) #F))
-}

multiple_of a b = mod b a == 0

sieve filter 1000 = []
sieve filter n = if filter n then 
                   sieve filter (n+1)
                 else
                   let
                     new_filter x = if multiple_of n x then
                                      True
                                    else
                                      filter x
                   in
                     n : (sieve new_filter (n+1))

false_function x = False

-- f a b = a : b
data Seq a = Nil | Cons (a, (() -> Seq a))
-- list a = Nil | Cons (a, list a)

instance (Show a) => Show (Seq a) where
  show Nil = "Nil"
  show (Cons (a, f)) = "Cons(" ++ show a ++ ", fn())"

hd Nil = error "HD_NIL"
hd (Cons (x, f)) = x

tl Nil = error "TL_NIL"
tl (Cons (x, f)) = f()

from k = Cons(k, \() -> from (k+1))

takeq 0 s = []
takeq n Nil = []
--takeq n (Cons(x, xf)) = x : takeq (n-1) (xf())
takeq n (Cons(x, xf)) = x : takeq (n-1) (tl (Cons(x, xf)))

sieve_seq filter (Cons(1000,_)) = []
sieve_seq filter seq = if filter (hd seq) then
                       sieve_seq filter (tl seq)
                     else
                       let
                         new_filter x = if multiple_of (hd seq) x then
                                          True
                                        else
                                          filter x
                      in
                        (hd seq) : (sieve_seq new_filter (tl seq))

