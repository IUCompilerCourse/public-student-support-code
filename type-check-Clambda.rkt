#lang racket
(require "utilities.rkt")
(require "type-check-Cfun.rkt")
(require "type-check-Llambda.rkt")
(provide type-check-Clambda type-check-Clambda-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Clambda

(define type-check-Clambda-class
  (class (type-check-lambda-mixin type-check-Cfun-class)
    (super-new)
    (inherit type-equal? exp-ready?)

    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(FunRef f n) (set)]
	[(Lambda (list `[,xs : ,Ts] ...) rT body)
         (define (rm x s) (set-remove s x))
         (foldl rm (recur body) xs)]
        [(AllocateClosure len ty arity) (set)]
        [else (super free-vars-exp e)]))
    
    ))

(define (type-check-Clambda p)
  (send (new type-check-Clambda-class) type-check-program p))



