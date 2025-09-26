#lang racket
(require "interp_Lvec_prime.rkt")
(require "interp_Lvecof_prime.rkt")
(require "interp_Lfun_prime.rkt")
(require "interp_Llambda_prime.rkt")
(require "interp_Lany.rkt")
(require "utilities.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp_Lany_prime interp_Lany_prime-class interp_Lany_prime-mixin)

(define (interp_Lany_prime-mixin super-class)
  (class super-class
    (super-new)

    (define/override (interp-op op)
      (verbose "Lany_prime/interp-op" op)
      (match op
        ['make-any (lambda (v tg) (Tagged v tg))]
        ['tag-of-any
         (match-lambda
           [(Tagged v^ tg)  tg]
           [v  (error 'interp-op "expected tagged value, not ~a" v)])]
        [else (super interp-op op)]))

    (define/override ((interp_exp env) e)
      (define recur (interp_exp env))
      (verbose "Lany_prime/interp_exp" e)
      (match e
        [(ValueOf e ty)
         (match (recur e)
           [(Tagged v^ tg)  v^]
           [v (error 'interp-op "expected tagged value, not ~a" v)])]
        [else ((super interp_exp env) e)]))
  ))

(define interp_Lany_prime-class
  (interp_Lany_prime-mixin
   (interp_Llambda_prime-mixin
    (interp_Lfun_prime-mixin
     (interp_Lvecof_prime-mixin
      (interp_Lvec_prime-mixin
       interp_Lany-class))))))
    
(define (interp_Lany_prime p)
  (send (new interp_Lany_prime-class) interp-program p))
