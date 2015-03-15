#lang typed/racket

(provide real-modulo)

(: real-modulo : [Real Integer -> Real])
(define (real-modulo x m)
  (define x-int (exact-floor x))
  (define x-rst (- x x-int))
  (define x-int%m (modulo x-int m))
  (define x%m (+ x-int%m x-rst))
  (cond [(< m x%m) (- x%m m)]
        [else x%m]))

