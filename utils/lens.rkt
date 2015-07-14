#lang afl postfix-dot-notation sweet-exp racket/base

provide struct/lens lens-transform**

require racket/splicing
        syntax/parse/define
        lens/applicable
        alexis/util/struct
        unstable/lens/struct
        for-syntax racket/base
                   syntax/parse
                   racket/syntax
module+ test
  require rackunit

define-simple-macro
  lens-transform** tgt:expr
    lens:expr old-v:id body:expr |...+|
    ...
  #:with [[lns/f ...] ...]
  #'[[lens (λ (old-v) body ...)] ...]
  (lens-transform/list tgt lns/f ... ...)

module+ test
  struct/lens foo (a b c) #:transparent
  define f (foo 1 2 3)
  check-equal? (lens-transform/list f foo-a-lens #λ(* 100 %)) (foo 100 2 3)
  check-equal? (lens-transform/list f foo-b-lens #λ(* 100 %)) (foo 1 200 3)
  check-equal? (lens-transform/list f foo-c-lens #λ(* 100 %)) (foo 1 2 300)
  
