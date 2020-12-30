#lang racket
(require "utilities.rkt")
(require "interp-Rany-prime.rkt")
(require "interp-Cvar.rkt")
(require "interp-Cif.rkt")
(require "interp-Cvec.rkt")
(require "interp-Cfun.rkt")
(require "interp-Clambda.rkt")
(provide interp-Cany)

(define Cany-class (interp-Clambda-mixin
                  (interp-Cfun-mixin
                   (interp-Cvec-mixin
                    (interp-Cif-mixin
                     (interp-Cvar-mixin
                      interp-Rany-prime-class))))))

(define (interp-Cany p)
  (send (new Cany-class) interp-program p))
