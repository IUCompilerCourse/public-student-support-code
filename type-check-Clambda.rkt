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

    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "Clambda" e)
        (define recur (type-check-exp env))
        (match e
          [(UncheckedCast e t)
           (define-values (new-e new-t) (recur e))
	   (values (UncheckedCast new-e t) t)]
          [else ((super type-check-exp env) e)])))

    (define/override (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(FunRef f n) (set)]
	[(Lambda (list `[,xs : ,Ts] ...) rT body)
         (define (rm x s) (set-remove s x))
         (foldl rm (recur body) xs)]
        [(AllocateClosure len ty arity) (set)]
	[(UncheckedCast e t) (recur e)]
        [else (super free-vars-exp e)]))
    
    ))

(define (type-check-Clambda p)
  (send (new type-check-Clambda-class) type-check-program p))



