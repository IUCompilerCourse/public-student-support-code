#lang racket
(require "utilities.rkt")
(require "interp_Lany_prime.rkt")
(require "interp_Cvar.rkt")
(require "interp_Cif.rkt")
(require "interp_Cwhile.rkt")
(require "interp_Cvec.rkt")
(require "interp_Cvecof.rkt")
(require "interp_Cfun.rkt")
(require "interp_Clambda.rkt")
(provide interp_Cany)

(define Cany-class (interp_Clambda-mixin
                    (interp_Cfun-mixin
                     (interp_Cvecof-mixin
                      (interp_Cvec-mixin
                       (interp_Cwhile-mixin
                        (interp_Cif-mixin
                         (interp_Cvar-mixin
                          interp_Lany_prime-class))))))))

(define (interp_Cany p)
  (send (new Cany-class) interp-program p))
