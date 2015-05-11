#lang sweet-exp racket/base

provide make-file-read-write-proc

define (make-file-read-write-proc path)
  when not(file-exists?(path))
    call-with-output-file* path #:exists 'error void
  define proc
    case-lambda
      ()
        call-with-input-file* path
          λ (in) read(in)
      (val)
        call-with-output-file* path #:exists 'replace
          λ (out) write(val out)
  proc

