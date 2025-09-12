#lang racket
(require "interp_Lvec_prime.rkt")
(require "interp_Lvecof_prime.rkt")
(require "interp_Lfun_prime.rkt")
(require "interp_Llambda.rkt")
(require "utilities.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp_Llambda_prime interp_Llambda_prime-mixin interp_Llambda_prime-class)

(define (interp_Llambda_prime-mixin super-class)
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
    
    (define/override ((interp_exp env) e)
      (verbose "Llambda'/interp_exp" e)
      (match e
        [(Closure arity args)
         (define arg-vals (map (interp_exp env) args))
         (apply vector (append arg-vals (list `(arity ,arity))))]
        [else ((super interp_exp env) e)]))
    ))

(define interp_Llambda_prime-class
  (interp_Llambda_prime-mixin
   (interp_Lfun_prime-mixin
    (interp_Lvecof_prime-mixin
     (interp_Lvec_prime-mixin
      interp_Llambda-class)))))
    
(define (interp_Llambda_prime p)
  (send (new interp_Llambda_prime-class) interp-program p))
