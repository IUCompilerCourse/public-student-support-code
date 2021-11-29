#lang racket
(require "utilities.rkt")
(require "interp-Lvecof-proxy-closure.rkt")
(require "interp-Cvar.rkt")
(require "interp-Cif.rkt")
(require "interp-Cvec.rkt")
(require "interp-Cfun.rkt")
(require "interp-Clambda.rkt")
(require "interp-Cwhile.rkt")
(provide interp-Cvecof interp-Cvecof-mixin interp-Cvecof-class)

(define (interp-Cvecof-mixin super-class)
  (class super-class
    (super-new)
    (inherit interp-exp)

    (define/override ((interp-stmt env) s)
      (match s
        [(Prim 'vectorof-set! (list e-vec i e-arg))
         ((interp-exp env) s)
         env]
        [else ((super interp-stmt env) s)]))

    ))


(define interp-Cvecof-class (interp-Cvecof-mixin
                             (interp-Cwhile-mixin
                              (interp-Clambda-mixin
                               (interp-Cfun-mixin
                                (interp-Cvec-mixin
                                 (interp-Cif-mixin
                                  (interp-Cvar-mixin
                                   interp-Lvecof-proxy-closure-class))))))))

(define (interp-Cvecof p)
  (send (new interp-Cvecof-class) interp-program p))


