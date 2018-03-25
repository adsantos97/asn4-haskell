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
