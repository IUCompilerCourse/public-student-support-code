#lang racket
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Cif.rkt")
(require "type-check-Cvec.rkt")
(require "type-check-Lfun.rkt")
(require "type-check-Cfun.rkt")
(provide type-check-Llambda type-check-Llambda-class typed-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Lambda                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Llambda

(define typed-vars (make-parameter #f))

(define type-check-Llambda-class
  (class type-check-Lfun-class
    (super-new)
    (inherit check-type-equal?)
    (inherit-field max-parameters)
    
    (define/override (type-check-exp env)
      (lambda (e)
        (debug 'type-check-exp "Llambda" e)
        (define recur (type-check-exp env))
        (match e
          [(HasType (Var x) t)
           ((type-check-exp env) (Var x))]
          [(Var x)
           (define t (dict-ref env x))
           (define var (cond [(typed-vars) (HasType (Var x) t)]
                             [else (Var x)]))
           (values var t)]
          [(Closure arity es)
           (define-values (e* t*) (for/lists (e* t*) ([e es])
                                    (recur e)))
           (let ([t `(Vector ,@t*)])
             (values (HasType (Closure arity e*) t) t))]
          [(Prim 'procedure-arity (list e1))
           (define-values (e1^ t) (recur e1))
           (match t
             ;; before closure conversion
             [`(,ts ... -> ,rt)
              (values (Prim 'procedure-arity (list e1^)) 'Integer)]
             ;; after closure conversion
             [`(Vector (,clos ,ts ... -> ,rt) ,ts2 ...)
              (values (Prim 'procedure-arity (list e1^)) 'Integer)]
             [else (error 'type-check
                          "expected a function not ~a\nin ~v" t e)])]
          [(HasType (Closure arity es) t)
           ((type-check-exp env) (Closure arity es))]
          [(AllocateClosure size t arity)
           (values (AllocateClosure size t arity) t)]
          [(FunRef f n)
           (let ([t (dict-ref env f)])
             (values (FunRef f n) t))]
          [(Lambda (and params `([,xs : ,Ts] ...)) rT body)
           (unless (< (length xs) max-parameters)
             (error 'type-check "lambda has too many parameters, max is ~a"
                    max-parameters))
           (define-values (new-body bodyT) 
             ((type-check-exp (append (map cons xs Ts) env)) body))
           (define ty `(,@Ts -> ,rT))
           (check-type-equal? rT bodyT e)
           (values (Lambda params rT new-body) ty)]
          [else ((super type-check-exp env) e)]
          )))

    ))

(define (type-check-Llambda p)
  (send (new type-check-Llambda-class) type-check-program p))
