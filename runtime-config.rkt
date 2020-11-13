#lang racket

(require racket/contract)
(provide
 (contract-out
  [rootstack-size (parameter/c exact-nonnegative-integer?)]
  [heap-size      (parameter/c exact-nonnegative-integer?)]))

;; We provide this interface so that we have a uniform means of
;; playing with the runtime configuration parameters of your compiler.
;; Please require this file and use these parameters when needing
;; to determining the rootstack-size and heap-size.

;; Parameter that determines what the initial rootstack size of the program is.
;; in order to get this value use                (rootstack-size)
;; in order to set this value to (expt 2 8) use  (rootstack-size (expt 2 8))
(define rootstack-size
  (make-parameter (expt 2 16)))

;; Parameter that determines what the initial heap size of the program is.
(define heap-size 
  (make-parameter (expt 2 16)))
