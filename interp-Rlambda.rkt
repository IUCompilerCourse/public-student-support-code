#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require "interp-Rfun.rkt")
(provide interp-Rlambda interp-Rlambda-class)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define interp-Rlambda-class
  (class interp-Rfun-class
    (super-new)

    (define/override (interp-op op)
      (verbose "Rlambda/interp-op" op)
      (match op
        ['procedure-arity
         (match-lambda
           [`(function (,xs ...) ,body ,lam-env)  (length xs)]
           [v (error 'interp-op "Rlambda/expected a function, not ~a" v)])]
        [else (super interp-op op)]))

    (define/override ((interp-exp env) e)
      (define recur (interp-exp env))
      (verbose "Rlambda/interp-exp" e)
      (match e
        [(Lambda (list `[,xs : ,Ts] ...) rT body)
         `(function ,xs ,body ,env)]
        [else ((super interp-exp env) e)]))
    ))

(define (interp-Rlambda p)
  (send (new interp-Rlambda-class) interp-program p))
 
