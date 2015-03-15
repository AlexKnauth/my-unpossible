#lang racket/base

(provide (all-from-out rackunit)
         check-within
         )

(require rackunit
         "equal-within.rkt"
         )

(define-check (check-within a b ∆)
  (check-true (equal?/within a b ∆)))

