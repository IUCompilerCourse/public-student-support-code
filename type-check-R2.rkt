#lang racket
(require "utilities.rkt")
(provide type-check-R2 type-check-C1 type-check-op type-equal?
         operator-types)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Booleans and Control Flow                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-R2

(define (operator-types)
  '((+ . ((Integer Integer) . Integer))
    (- . ((Integer Integer) . Integer))
    (and . ((Boolean Boolean) . Boolean))
    (or . ((Boolean Boolean) . Boolean))
    (< . ((Integer Integer) . Boolean))
    (<= . ((Integer Integer) . Boolean))
    (> . ((Integer Integer) . Boolean))
    (>= . ((Integer Integer) . Boolean))
    (- . ((Integer) . Integer))
    (not . ((Boolean) . Boolean))
    (read . (() . Integer))
    ))

(define (type-equal? t1 t2)
  (equal? t1 t2))

(define (type-check-op op arg-types)
  (match (dict-ref (operator-types) op)
    [`(,param-types . ,return-type)
     (for ([at arg-types] [pt param-types]) 
       (unless (type-equal? at pt)
         (error 'type-check-op
                "type error: argument type ~a not equal to parameter type ~a"
                at pt)))
     return-type]
    [else
     (error 'type-check-op "unrecognized operator ~a" op)]))

(define (type-check-exp env)
  (lambda (e)
    (match e
      [(Var x)
       (let ([t (dict-ref env x)])
         (values (Var x) t))]
      [(Int n) (values (Int n) 'Integer)]
      [(Bool b) (values (Bool b) 'Boolean)]
      [(Let x e body)
       (define-values (e^ Te) ((type-check-exp env) e))
       (define-values (b Tb) ((type-check-exp (dict-set env x Te)) body))
       (values (Let x e^ b) Tb)]
      [(If cnd thn els)
       (define-values (c Tc) ((type-check-exp env) cnd))
       (define-values (t Tt) ((type-check-exp env) thn))
       (define-values (e Te) ((type-check-exp env) els))
       (unless (type-equal? Tc 'Boolean)
         (error 'type-check-exp "condition should be Boolean, not ~a" Tc))
       (unless (type-equal? Tt Te)
         (error 'type-check-exp "types of branches not equal, ~a != ~a" Tt Te))
       (values (If c t e) Te)]
      [(Prim 'eq? (list e1 e2))
       (define-values (e1^ T1) ((type-check-exp env) e1))
       (define-values (e2^ T2) ((type-check-exp env) e2))
       (unless (type-equal? T1 T2)
         (error 'type-check-exp "argument types of eq?: ~a != ~a" T1 T2))
       (values (Prim 'eq? (list e1^ e2^)) 'Boolean)]
      [(Prim op es)
        (define-values (new-es ts)
          (for/lists (exprs types) ([e es]) ((type-check-exp env) e)))
        (define t-ret (type-check-op op ts))
        (values (Prim op new-es) t-ret)]
      [else
       (error 'type-check-exp "couldn't match" e)])))

(define (type-check-R2 e)
  (match e
    [(Program info body)
     (define-values (body^ Tb) ((type-check-exp '()) body))
     (unless (type-equal? Tb 'Integer)
       (error 'type-check-R2 "result type must be Integer, not ~a" Tb))
     (Program info body^)]
    [else (error 'type-check-R2 "couldn't match ~a" e)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-C1

(define (type-check-stmt env)
  (lambda (s)
    (match s
      [(Assign (Var x) e)
       (define-values (e^ t) ((type-check-exp env) e))
       (cond [(dict-has-key? env x)
              (unless (type-equal? t (dict-ref env x))
                (error 'type-check-stmt
                       "variable and RHS have different types, ~a != ~a"
                       t (dict-ref env x)))]
             [else
              (dict-set! env x t)])]
      )))

(define (type-check-tail env block-env G)
  (lambda (t)
    (match t
      [(Return e)
       (define-values (e^ t) ((type-check-exp env) e))
       t]
      [(Seq s t)
       ((type-check-stmt env) s)
       ((type-check-tail env block-env G) t)]
      [(Goto label)
       ;; Memoization because G is a DAG -Jeremy
       (cond [(dict-has-key? block-env label)
              (dict-ref block-env label)]
             [else
              (define t ((type-check-tail env block-env G)
                         (dict-ref G label)))
              (dict-set! block-env label t)
              t])]
      [(IfStmt cnd tail1 tail2)
       (define-values (c Tc) ((type-check-exp env) cnd))
       (unless (type-equal? Tc 'Boolean)
         (error "type error: condition of if should be Boolean, not" Tc))
       (define T1 ((type-check-tail env block-env G) tail1))
       (define T2 ((type-check-tail env block-env G) tail2))
       (unless (type-equal? T1 T2)
         (error "type error: branches of if should have same type, not"
                T1 T2))
       T1]
      )))

(define (type-check-C1 p)
  (match p
    [(Program info (CFG G))
     ;; Top-down traversal so we see variable definitions before uses.
     ;; -Jeremy
     (define env (make-hash))
     (define block-env (make-hash))
     (define t ((type-check-tail env block-env G)
                (dict-ref G 'start)))
     (unless (type-equal? t 'Integer)
       (error "return type of program must be Integer, not" t))
     (define locals-types (for/list ([(x t) (in-dict env)])
                                 (cons x t)))
     (define new-info (dict-set info 'locals-types locals-types))
     (Program new-info (CFG G))]))
