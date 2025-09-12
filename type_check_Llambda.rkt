#lang racket
(require "utilities.rkt")
(require "type_check_Lvec.rkt")
(require "type_check_Lvecof.rkt")
(require "type_check_Lfun.rkt")
(provide type_check_Llambda type_check_Llambda-has-type
         type_check_Llambda-class type_check_lambda-mixin typed-vars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Lambda                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_lambda-mixin (for use in Llambda and Clambda)

(define (type_check_lambda-mixin super-class)
  (class super-class
    (super-new)
    (inherit check-type-equal?)

    (define/override (type_check_exp env)
      (lambda (e)
        (debug 'type_check_exp "Llambda" e)
        (define recur (type_check_exp env))
        (match e
          [(AllocateClosure size t arity)
           (values (AllocateClosure size t arity) t)]
          [(Prim 'procedure-arity (list e1))
           (define-values (e1^ t) (recur e1))
           (match t
             ;; after closure conversion
             [`(Vector (,clos ,ts ... -> ,rt) ,ts2 ...)
              (values (Prim 'procedure-arity (list e1^)) 'Integer)]
             [else (error 'type_check
                          "expected a function not ~a\nin ~v" t e)])]
          [else ((super type_check_exp env) e)]
          )))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type_check_Llambda

(define typed-vars (make-parameter #f))

(define type_check_Llambda-class
  (class (type_check_lambda-mixin type_check_Lfun-class)
    (super-new)
    (inherit check-type-equal?)
    (inherit-field max-parameters)
    
    (define/public (closure-type fun-ty)
      (match fun-ty
	[`(,clos ,ps ... -> ,rt)
	 `(Vector ((Vector _) ,@ps -> ,rt))]
	[else (error "closure-type, expected function type")]))
    
    (define/override (type_check_exp env)
      (lambda (e)
        (debug 'type_check_exp "Llambda" e)
        (define recur (type_check_exp env))
        (match e
          [(HasType (Var x) t)
           ((type_check_exp env) (Var x))]
          [(Var x)
           (define t (dict-ref env x))
           (define var (cond [(typed-vars) (HasType (Var x) t)]
                             [else (Var x)]))
           (values var t)]
          [(Closure arity es)
           (define-values (e* t*) (for/lists (e* t*) ([e es])
					     (recur e)))
           (let ([ct (closure-type (car t*))])
	     ;; The following is an abuse of HasType to transport
	     ;; information to the expose-allocation pass. -Jeremy
             (values (HasType (Closure arity e*) `(Vector ,@t*)) ct))]
          [(Prim 'procedure-arity (list e1))
           (define-values (e1^ t) (recur e1))
           (match t
             ;; before closure conversion
             [`(,ts ... -> ,rt)
              (values (Prim 'procedure-arity (list e1^)) 'Integer)]
             [else ((super type_check_exp env) e)])]
          [(HasType (Closure arity es) t)
           ((type_check_exp env) (Closure arity es))]
          [(UncheckedCast e t)
           (define-values (new-e new-t) (recur e))
	   (values (UncheckedCast new-e t) t)]
          [(FunRef f n)
           (let ([t (dict-ref env f)])
             (values (FunRef f n) t))]
          [(Lambda (and params `([,xs : ,Ts] ...)) rT body)
           (unless (< (length xs) max-parameters)
             (error 'type_check "lambda has too many parameters, max is ~a"
                    max-parameters))
           (define-values (new-body bodyT) 
             ((type_check_exp (append (map cons xs Ts) env)) body))
           (define ty `(,@Ts -> ,rT))
           (check-type-equal? rT bodyT e)
           (values (Lambda params rT new-body) ty)]
          [else ((super type_check_exp env) e)]
          )))

    ))

(define (type_check_Llambda p)
  (send (new type_check_Llambda-class) type_check_program p))

(define (type_check_Llambda-has-type p)
  (begin
    (typed-vec #t)
    (typed-vecof #t)
    (define t (type_check_Llambda p))
    (typed-vec #f)
    (typed-vecof #f)
    t))

