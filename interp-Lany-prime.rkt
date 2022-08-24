#lang racket
(require "interp-Lvec-prime.rkt")
(require "interp-Lvecof-prime.rkt")
(require "interp-Lfun-prime.rkt")
(require "interp-Llambda-prime.rkt")
(require "interp-Lany.rkt")
(require "utilities.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-Lany-prime interp-Lany-prime-class interp-Lany-prime-mixin)

(define (interp-Lany-prime-mixin super-class)
  (class super-class
    (super-new)

    (define/override (interp-op op)
      (verbose "Lany-prime/interp-op" op)
      (match op
        ['make-any (lambda (v tg) (Tagged v tg))]
        ['tag-of-any
         (match-lambda
           [(Tagged v^ tg)  tg]
           [v  (error 'interp-op "expected tagged value, not ~a" v)])]
        [else (super interp-op op)]))

    (define/override ((interp-exp env) e)
      (define recur (interp-exp env))
      (verbose "Lany-prime/interp-exp" e)
      (match e
        [(ValueOf e ty)
         (match (recur e)
           [(Tagged v^ tg)  v^]
           [v (error 'interp-op "expected tagged value, not ~a" v)])]
        [else ((super interp-exp env) e)]))
  ))

(define interp-Lany-prime-class
  (interp-Lany-prime-mixin
   (interp-Llambda-prime-mixin
    (interp-Lfun-prime-mixin
     (interp-Lvecof-prime-mixin
      (interp-Lvec-prime-mixin
       interp-Lany-class))))))
    
(define (interp-Lany-prime p)
  (send (new interp-Lany-prime-class) interp-program p))
