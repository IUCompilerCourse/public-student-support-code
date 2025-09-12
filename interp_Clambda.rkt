#lang racket
(require "utilities.rkt")
(require "interp_Llambda_prime.rkt")
(require "interp_Cvar.rkt")
(require "interp_Cif.rkt")
(require "interp_Cwhile.rkt")
(require "interp_Cvec.rkt")
(require "interp_Cvecof.rkt")
(require "interp_Cfun.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp_Clambda interp_Clambda-mixin)

(define (interp_Clambda-mixin super-class)
  (class super-class
    (super-new)
    
    (define/override (interp-op op)
      (verbose "Clambda/interp-op" op)
      (match op
        ['procedure-arity
         (match-lambda
           [(vector (CFunction xs info G env) vs ... `(arity ,n)) n]
           [v (error 'interp-op "Clambda/expected function, not ~a" v)])]
        [else (super interp-op op)]))
    ))

(define Clambda-class (interp_Clambda-mixin
                       (interp_Cfun-mixin
                        (interp_Cvecof-mixin
                         (interp_Cvec-mixin
                          (interp_Cwhile-mixin
                           (interp_Cif-mixin
                            (interp_Cvar-mixin
                             interp_Llambda_prime-class))))))))
  
(define (interp_Clambda p)
  (send (new Clambda-class) interp-program p))
