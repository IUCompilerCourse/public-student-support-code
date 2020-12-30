#lang racket
(require "utilities.rkt")
(require "interp-Rlambda-prime.rkt")
(require "interp-Cvar.rkt")
(require "interp-Cif.rkt")
(require "interp-Cvec.rkt")
(require "interp-Cfun.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-Clambda interp-Clambda-mixin)

(define (interp-Clambda-mixin super-class)
  (class super-class
    (super-new)
    
    (define/override (interp-op op)
      (verbose "Clambda/interp-op" op)
      (match op
        ['procedure-arity
         (match-lambda
           [(vector `(function ,xs ,info ,G ,env) vs ... `(arity ,n)) n]
           [v (error 'interp-op "Clambda/expected function, not ~a" v)])]
        [else (super interp-op op)]))
    ))

(define Clambda-class (interp-Clambda-mixin
                       (interp-Cfun-mixin
                        (interp-Cvec-mixin
                         (interp-Cif-mixin
                          (interp-Cvar-mixin
                           interp-Rlambda-prime-class))))))

(define (interp-Clambda p)
  (send (new Clambda-class) interp-program p))
