#lang racket
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Cif.rkt")
(require "type-check-Cvec.rkt")
(require "type-check-Cfun.rkt")
(require "type-check-Rlambda.rkt")
(provide type-check-Clambda type-check-Clambda-class)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Clambda

(define type-check-Clambda-class (type-check-Cfun-mixin
                                  (type-check-Cvec-mixin
                                   (type-check-Cif-mixin
                                    (type-check-Cvar-mixin
                                     type-check-Rlambda-class)))))

(define (type-check-Clambda p)
  (send (new type-check-Clambda-class) type-check-program p))



