#lang sweet-exp typed/racket/base

provide equal?/within

require racket/flonum
        my-cond/iffy

: equal?/within : Any Any Nonnegative-Real -> Boolean
define equal?/within(a b ∆)
  : equal-proc : Any Any -> Boolean
  define equal-proc(a b)
    my-cond
      if {number?(a) and number?(b)}
        {magnitude{a - b} <= ∆}
      else-if {flvector?(a) and flvector?(b)}
        and {flvector-length(a) = flvector-length(b)}
            for/and ([a in-flvector(a)] [b in-flvector(b)])
              equal-proc(a b)
      else
        (equal?/recur a b equal-proc)
  equal-proc(a b)

