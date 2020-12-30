#lang racket
(require "interp-Rvec-prime.rkt")
(require "interp-Rfun-prime.rkt")
(require "interp-Rlambda.rkt")
(require "utilities.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-Rlambda-prime interp-Rlambda-prime-mixin interp-Rlambda-prime-class)

(define (interp-Rlambda-prime-mixin super-class)
  (class super-class
    (super-new)

    (define/override (interp-op op)
      (verbose "Rlambda'/interp-op" op)
      (match op
        ['procedure-arity
         (match-lambda
           [`(function ,xs ,body ,env) (length xs)]
           [(vector `(function ,xs ,body ,env) vs ... `(arity ,n)) n]
           [v (error 'interp-op "Rlambda'/expected function, not ~a" v)])]
        [else (super interp-op op)]))
    
    (define/override ((interp-exp env) e)
      (verbose "Rlambda'/interp-exp" e)
      (match e
        [(Closure arity args)
         (define arg-vals (map (interp-exp env) args))
         (apply vector (append arg-vals (list `(arity ,arity))))]
        [else ((super interp-exp env) e)]))
    ))

(define interp-Rlambda-prime-class
  (interp-Rlambda-prime-mixin
   (interp-Rfun-prime-mixin
    (interp-Rvec-prime-mixin interp-Rlambda-class))))
    
(define (interp-Rlambda-prime p)
  (send (new interp-Rlambda-prime-class) interp-program p))
