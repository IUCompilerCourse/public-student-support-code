#lang racket
(require "interp-Rvec-prime.rkt")
(require "interp-Rfun-prime.rkt")
(require "interp-Rlambda-prime.rkt")
(require "interp-Rany.rkt")
(require "utilities.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-Rany-prime interp-Rany-prime-class interp-Rany-prime-mixin)

(define (interp-Rany-prime-mixin super-class)
  (class super-class
    (super-new)

    (define/override (interp-op op)
      (verbose "Rany-prime/interp-op" op)
      (match op
        ['make-any (lambda (v tg) (Tagged v tg))]
        ['tag-of-any
         (match-lambda
           [(Tagged v^ tg)  tg]
           [v  (error 'interp-op "expected tagged value, not ~a" v)])]
        [else (super interp-op op)]))

    (define/override ((interp-exp env) e)
      (define recur (interp-exp env))
      (verbose "Rany-prime/interp-exp" e)
      (match e
        [(ValueOf e ty)
         (match (recur e)
           [(Tagged v^ tg)  v^]
           [v (error 'interp-op "expected tagged value, not ~a" v)])]
        [(Exit) (error 'interp-exp "exiting")]
        [else ((super interp-exp env) e)]))
  ))

(define interp-Rany-prime-class
  (interp-Rany-prime-mixin
   (interp-Rlambda-prime-mixin
    (interp-Rfun-prime-mixin
     (interp-Rvec-prime-mixin interp-Rany-class)))))
    
(define (interp-Rany-prime p)
  (send (new interp-Rany-prime-class) interp-program p))
