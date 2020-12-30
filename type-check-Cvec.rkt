#lang racket
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Cif.rkt")
(require "type-check-Rvec.rkt")
(provide type-check-Cvec type-check-Cvec-mixin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cvec

(define (type-check-Cvec-mixin super-class)
  (class super-class
    (super-new)
    (inherit type-check-exp check-type-equal?)

    (define/override ((type-check-stmt env) s)
      (match s
        [(Collect size) (void)]
        [else ((super type-check-stmt env) s)]))
    ))

(define type-check-Cvec-class (type-check-Cvec-mixin
                             (type-check-Cif-mixin
                              (type-check-Cvar-mixin
                               type-check-Rvec-class))))

(define (type-check-Cvec p)
  (send (new type-check-Cvec-class) type-check-program p))


  
