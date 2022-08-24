#lang racket
(require "interp-Lvec-prime.rkt")
(require "interp-Lvecof-prime.rkt")
(require "interp-Lfun-prime.rkt")
(require "interp-Llambda.rkt")
(require "utilities.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-Llambda-prime interp-Llambda-prime-mixin interp-Llambda-prime-class)

(define (interp-Llambda-prime-mixin super-class)
  (class super-class
    (super-new)

    (define/override (interp-op op)
      (verbose "Llambda'/interp-op" op)
      (match op
        ['procedure-arity
         (match-lambda
           [(Function xs body env) (length xs)]
           [(vector (Function xs body env) vs ... `(arity ,n)) n]
           [v (error 'interp-op "Llambda'/expected function, not ~a" v)])]
        [else (super interp-op op)]))
    
    (define/override ((interp-exp env) e)
      (verbose "Llambda'/interp-exp" e)
      (match e
        [(Closure arity args)
         (define arg-vals (map (interp-exp env) args))
         (apply vector (append arg-vals (list `(arity ,arity))))]
        [else ((super interp-exp env) e)]))
    ))

(define interp-Llambda-prime-class
  (interp-Llambda-prime-mixin
   (interp-Lfun-prime-mixin
    (interp-Lvecof-prime-mixin
     (interp-Lvec-prime-mixin
      interp-Llambda-class)))))
    
(define (interp-Llambda-prime p)
  (send (new interp-Llambda-prime-class) interp-program p))
