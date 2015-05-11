#lang sweet-exp typed/racket/base

provide make-file-read-write-proc

: make-file-read-write-proc : Path-String -> (case-> [-> Any] [Any -> Void])
define (make-file-read-write-proc path)
  when not(file-exists?(path))
    call-with-output-file* path #:exists 'error void
  : proc : (case-> [-> Any] [Any -> Void])
  define proc
    case-lambda
      ()
        call-with-input-file* path
          λ ([in : Input-Port]) read(in)
      (val)
        call-with-output-file* path #:exists 'replace
          λ ([out : Output-Port]) write(val out)
  proc

