#lang racket
(require "utilities.rkt")
(require "interp-Lany-prime.rkt")
(require "interp-Cvar.rkt")
(require "interp-Cif.rkt")
(require "interp-Cwhile.rkt")
(require "interp-Cvec.rkt")
(require "interp-Cvecof.rkt")
(require "interp-Cfun.rkt")
(require "interp-Clambda.rkt")
(provide interp-Cany)

(define Cany-class (interp-Clambda-mixin
                    (interp-Cfun-mixin
                     (interp-Cvecof-mixin
                      (interp-Cvec-mixin
                       (interp-Cwhile-mixin
                        (interp-Cif-mixin
                         (interp-Cvar-mixin
                          interp-Lany-prime-class))))))))

(define (interp-Cany p)
  (send (new Cany-class) interp-program p))
