#lang racket/base

(provide with-loc)

(require (for-syntax racket/base syntax/parse))
(module+ test
  (require rackunit syntax/location))

(define-syntax with-loc
  (syntax-parser
    [(with-loc loc stx)
     (datum->syntax #'stx (syntax-e #'stx) #'loc #'stx)]))

(module+ test
  (check-equal? (with-loc here-on-line-15
                          (quote-line-number))
                15)
  )
