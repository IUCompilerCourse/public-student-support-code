#lang racket
(require "utilities.rkt")
(require "type_check_Cfun.rkt")
(require "type_check_Llambda.rkt")
(provide type_check_Clambda type_check_Clambda-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_Clambda

(define type_check_Clambda-class
  (class (type_check_lambda-mixin type_check_Cfun-class)
    (super-new)
    (inherit type-equal? exp-ready?)

    (define/override (type_check_exp env)
      (lambda (e)
        (debug 'type_check_exp "Clambda" e)
        (define recur (type_check_exp env))
        (match e
          [(UncheckedCast e t)
           (define-values (new-e new-t) (recur e))
	   (values (UncheckedCast new-e t) t)]
          [else ((super type_check_exp env) e)])))

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

(define (type_check_Clambda p)
  (send (new type_check_Clambda-class) type_check_program p))



