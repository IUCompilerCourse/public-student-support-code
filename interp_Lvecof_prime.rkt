#lang racket
(require "interp_Lvec_prime.rkt")
(require "interp_Lvecof.rkt")
(require "utilities.rkt")
(provide interp_Lvecof_prime interp_Lvecof_prime-mixin interp_Lvecof_prime-class)

(define (interp_Lvecof_prime-mixin super-class)
  (class super-class
    (super-new)
    (inherit-field uninitialized)

    (define/override (interp_exp env)
      (lambda (ast)
        (define recur (interp_exp env))
	(verbose "interp_exp" ast)
	(match ast
          [(AllocateArray e-len elt-ty)
           (build-vector (recur e-len) (lambda a uninitialized))]
	  [else ((super interp_exp env) ast)]
	  )))

    ))
    
(define interp_Lvecof_prime-class
  (interp_Lvecof_prime-mixin  
   (interp_Lvec_prime-mixin interp_Lvecof-class)))
    
(define (interp_Lvecof_prime p)
  (send (new interp_Lvecof_prime-class) interp-program p))
