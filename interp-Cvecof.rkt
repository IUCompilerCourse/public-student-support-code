#lang racket
(require "utilities.rkt")
(require "interp-Cvar.rkt")
(require "interp-Cif.rkt")
(require "interp-Cvec.rkt")
(require "interp-Cwhile.rkt")
(require "interp-Cvec.rkt")
(require "interp-Lvecof-prime.rkt")
(provide interp-Cvecof interp-Cvecof-mixin interp-Cvecof-class)

(define (interp-Cvecof-mixin super-class)
  (class super-class
    (super-new)
    (inherit interp-exp)

    (define/override ((interp-stmt env) s)
      (match s
        #;[(Prim 'vectorof-set! (list e-vec i e-arg))
         ((interp-exp env) s)
         env]
        [else ((super interp-stmt env) s)]))

    ))

(define interp-Cvecof-class
  (interp-Cvecof-mixin
   (interp-Cvec-mixin
    (interp-Cwhile-mixin
     (interp-Cif-mixin
      (interp-Cvar-mixin
       interp-Lvecof-prime-class))))))

(define (interp-Cvecof p)
  (send (new interp-Cvecof-class) interp-program p))


