#lang typed/racket/base

(provide from-to my-point-at)

(require pict3d)

(: from-to : [Affine Affine -> Affine])
(define (from-to t1 t2)
  (affine-compose t2 (affine-inverse t1)))

(: my-point-at : [Pos (U Pos Dir) Pos (U Pos Dir) [#:normalize? Boolean] -> Affine])
(define (my-point-at v1 v2 v3 v4 #:normalize? [normalize? #t])
  (from-to
   (point-at v1 v2 #:normalize? normalize?)
   (point-at v3 v4 #:normalize? normalize?)))

(module* test racket/base
  (require (submod "..") rackunit pict3d racket/match syntax/parse/define (for-syntax racket/base))
  (define-simple-macro (check-equal?/loc loc a b)
    #:with stx (syntax/loc #'loc (check-equal? a b))
    stx)
  (define-simple-macro
    (chk t-expr (~and ps [p1 p2]) ...+)
    (test-begin (define t t-expr)
                (check-equal?/loc ps (transform-pos p1 t) p2) ...))
  (check-equal? (my-point-at (pos 0 0 0) (pos 1 0 0)
                             (pos 0 0 0) (pos 1 0 0))
                identity-affine)
  (chk (my-point-at (pos 0 0 0) (pos 1 0 0)
                    (pos 0 0 0) (pos 1 0 0))
       [(pos 0 0 0) (pos 0 0 0)]
       [(pos 1 0 0) (pos 1 0 0)]
       [(pos 1/2 0 0) (pos 1/2 0 0)]
       )
  (chk (my-point-at (pos 0 0 0) (pos 1 0 0)
                    (pos 0 0 0) (pos 2 2 1) #:normalize? #f)
       [(pos 0 0 0) (pos 0 0 0)]
       [(pos 1 0 0) (pos 2 2 1)]
       [(pos 1/2 0 0) (pos 1 1 1/2)]
       )
  (chk (my-point-at (pos 0 0 0) (pos 1 0 0)
                    (pos 0 0 0) (pos 2 2 1) #:normalize? #t)
       ;; 2^2 + 2^2 + 1^2 = 3^2
       [(pos 0 0 0) (pos 0 0 0)]
       [(pos 1 0 0) (pos 2/3 2/3 1/3)]
       [(pos 1/2 0 0) (pos 1/3 1/3 1/6)]
       )
  (chk (my-point-at (pos 0 0 0) (pos 1 0 0)
                    (pos 1 2 3) (dir 2 2 1) #:normalize? #f)
       [(pos 0 0 0) (pos 1 2 3)]
       [(pos 1 0 0) (pos 3 4 4)]
       [(pos 1/2 0 0) (pos 2 3 (+ 3 1/2))]
       )
  (chk (my-point-at (pos 0 0 0) (pos 1 0 0)
                    (pos 1 2 3) (dir 2 2 1) #:normalize? #t)
       ;; 2^2 + 2^2 + 1^2 = 3^2
       [(pos 0 0 0) (pos 1 2 3)]
       ;[(pos 1 0 0) (pos (+ 1 2/3) (+ 2 2/3) (+ 3 1/3))] ; a bit of fl error
       [(pos 1/2 0 0) (pos (+ 1 1/3) (+ 2 1/3) (+ 3 1/6))]
       )
  (for ([i (in-range 10)])
    (match-define (list v1 v2 v3 v4)
      (for/list ([i (in-range 4)])
        (apply pos (for/list ([i (in-range 3)]) (random)))))
    (check-equal? (my-point-at (pos 0 0 0) (pos 0 0 1) v1 v2)
                  (point-at v1 v2))
    (check-equal? (affine-inverse (my-point-at v1 v2 v3 v4))
                  (my-point-at v3 v4 v1 v2)))
  )

