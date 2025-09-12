#lang racket
(require "utilities.rkt")
(require "interp_Cvar.rkt")
(require "interp_Cif.rkt")
(require "interp_Cvec.rkt")
(require "interp_Cwhile.rkt")
(require "interp_Cvec.rkt")
(require "interp_Lvecof_prime.rkt")
(provide interp_Cvecof interp_Cvecof-mixin interp_Cvecof-class)

(define (interp_Cvecof-mixin super-class)
  (class super-class
    (super-new)
    (inherit interp_exp)

    (define/override ((interp-stmt env) s)
      (match s
        #;[(Prim 'vectorof-set! (list e-vec i e-arg))
         ((interp_exp env) s)
         env]
        [else ((super interp-stmt env) s)]))

    ))

(define interp_Cvecof-class
  (interp_Cvecof-mixin
   (interp_Cvec-mixin
    (interp_Cwhile-mixin
     (interp_Cif-mixin
      (interp_Cvar-mixin
       interp_Lvecof_prime-class))))))

(define (interp_Cvecof p)
  (send (new interp_Cvecof-class) interp-program p))


