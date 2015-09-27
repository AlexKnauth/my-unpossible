#lang afl postfix-dot-notation sweet-exp racket/base

provide struct/lens lens-transform**

require racket/splicing
        syntax/parse/define
        lens/applicable
        alexis/util/struct
        for-syntax racket/base
                   syntax/parse
                   racket/syntax
module+ test
  require rackunit fancy-app

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
  check-equal? (lens-transform/list f foo-a-lens (* 100 _)) (foo 100 2 3)
  check-equal? (lens-transform/list f foo-b-lens (* 100 _)) (foo 1 200 3)
  check-equal? (lens-transform/list f foo-c-lens (* 100 _)) (foo 1 2 300)
  
