#lang racket
(require "interp-Lvec-prime.rkt")
(require "interp-Lvecof.rkt")
(require "utilities.rkt")
(provide interp-Lvecof-prime interp-Lvecof-prime-mixin interp-Lvecof-prime-class)

(define (interp-Lvecof-prime-mixin super-class)
  (class super-class
    (super-new)
    (inherit-field uninitialized)

    (define/override (interp-exp env)
      (lambda (ast)
        (define recur (interp-exp env))
	(verbose "interp-exp" ast)
	(match ast
          [(AllocateArray e-len elt-ty)
           (build-vector (recur e-len) (lambda a uninitialized))]
	  [else ((super interp-exp env) ast)]
	  )))

    ))
    
(define interp-Lvecof-prime-class
  (interp-Lvecof-prime-mixin  
   (interp-Lvec-prime-mixin interp-Lvecof-class)))
    
(define (interp-Lvecof-prime p)
  (send (new interp-Lvecof-prime-class) interp-program p))
