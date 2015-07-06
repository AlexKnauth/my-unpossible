#lang afl postfix-dot-notation sweet-exp racket/base

provide struct/lens lens-transform**

require racket/splicing
        syntax/parse/define
        lenses/applicable
        only-in lenses lens-transform*
        alexis/util/struct
        for-syntax racket/base
                   syntax/parse
                   racket/syntax
module+ test
  require rackunit

define-simple-macro
  defrename [id1 id2] ...
  begin
    define-syntax id1 (make-rename-transformer #'id2)
    ...

define-simple-macro
  struct/lens s:id (fld:id ...) option ...
  #:with [[s-fld s-fld-set] ...]
  for/list ([fld (in-list (syntax->list #'(fld ...)))])
    list (format-id #'s "~a-~a" #'s fld #:source fld)
         (format-id #'s "~a-~a-set" #'s fld #:source fld)
  #:with [-s -s-fld ...]
  (generate-temporaries #'[s s-fld ...])
  #:with def-struct-updaters
  (datum->syntax #'s `(,#'define-struct-updaters ,(syntax-e #'s)))
  splicing-local
    group
      splicing-local [(struct s (fld ...) option ...)]
        def-struct-updaters
        defrename [-s s] [-s-fld s-fld] ...
    defrename [s -s]
    define s-fld
      make-lens -s-fld s-fld-set
    ...

define-simple-macro
  lens-transform** tgt:expr
    lens:expr old-v:id body:expr |...+|
    ...
  #:with [[lns/f ...] ...]
  #'[[lens (λ (old-v) body ...)] ...]
  (lens-transform* tgt lns/f ... ...)

module+ test
  struct/lens foo (a b c) #:transparent
  define f (foo 1 2 3)
  check-equal? (lens-transform* f foo-a #λ(* 100 %)) (foo 100 2 3)
  check-equal? (lens-transform* f foo-b #λ(* 100 %)) (foo 1 200 3)
  check-equal? (lens-transform* f foo-c #λ(* 100 %)) (foo 1 2 300)
  
