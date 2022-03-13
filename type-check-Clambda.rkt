#lang racket
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Cif.rkt")
(require "type-check-Cwhile.rkt")
(require "type-check-Cvec.rkt")
(require "type-check-Cfun.rkt")
(require "type-check-Llambda.rkt")
(provide type-check-Clambda type-check-Clambda-class type-check-Clambda-mixin)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Clambda

(define (type-check-Clambda-mixin super-class)
  (class super-class
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
    
    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "Clambda" e)
        (define recur (type-check-exp env))
        (match e
          [(AllocateClosure size t arity)
           (values (AllocateClosure size t arity) t)]
          [(Prim 'procedure-arity (list e1))
           (define-values (e1^ t) (recur e1))
           (match t
             [`(Vector (,clos ,ts ... -> ,rt) ,ts2 ...)
              (values (Prim 'procedure-arity (list e1^)) 'Integer)]
             [else (error 'type-check
                          "expected a function not ~a\nin ~v" t e)])]
          [else ((super type-check-exp env) e)])))

    ))

(define type-check-Clambda-class
  (type-check-Clambda-mixin
   (type-check-Cfun-mixin
    (type-check-Cvec-mixin
     (type-check-Cwhile-mixin
      (type-check-Cif-mixin
       (type-check-Cvar-mixin
        type-check-Llambda-class)))))))
      
(define (type-check-Clambda p)
  (send (new type-check-Clambda-class) type-check-program p))



