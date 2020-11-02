#lang racket
(require "utilities.rkt")
(require (only-in "type-check-R2.rkt" type-check-op type-equal?))
(provide type-check-R3 type-check-C2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Tuples (aka Vectors)                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-R3

(define (type-check-exp env)
  (lambda (e)
    (define recur (type-check-exp env))
    (match e
      [(Var x)
       (let ([t (dict-ref env x)])
         (values (Var x) t))]
      [(Int n) (values (Int n) 'Integer)]
      [(Bool b) (values (Bool b) 'Boolean)]
      [(Let x e body)
       (define-values (e^ Te) (recur e))
       (define-values (b Tb) ((type-check-exp (cons `(,x . ,Te) env)) body))
       (values (Let x e^ b) Tb)]
      [(If cnd thn els)
       (define-values (c Tc) (recur cnd))
       (define-values (t Tt) (recur thn))
       (define-values (e Te) (recur els))
       (unless (type-equal? Tc 'Boolean)
         (error 'type-check-exp
                "expected condition of if to have type Boolean, not ~a" Tc))
       (unless (type-equal? Tt Te)
         (error 'type-check-exp
                "branches of if must have the same type, but ~a != ~a" Tt Te))
       (values (If c t e) Te)]
      [(Prim 'eq? (list e1 e2))
       (define-values (e1^ T1) (recur e1))
       (define-values (e2^ T2) (recur e2))
       (unless (type-equal? T1 T2)
         (error 'type-check-exp
                "arguments of eq? must have the same type, but ~a != ~a" T1 T2))
       (values (Prim 'eq? (list e1^ e2^)) 'Boolean)]
      [(Void) (values (Void) 'Void)]
      [(Prim 'vector es)
       (define-values (e* t*) (for/lists (e* t*) ([e es])
                                (recur e)))
       (let ([t `(Vector ,@t*)])
         (values (HasType (Prim 'vector e*) t) t))]
      [(Prim 'vector-length (list e))
       (define-values (e^ t) (recur e))
       (match t
         [`(Vector ,ts ...)
          (values (Prim 'vector-length (list e^))  'Integer)]
         [else (error 'type-check-exp
                      "expected a vector in vector-lenfth, not ~a" t)])]
      [(Prim 'vector-ref (list e (Int i)))
       (define-values (e^ t) (recur e))
       (match t
         [`(Vector ,ts ...)
          (unless (and (exact-nonnegative-integer? i) (< i (length ts)))
            (error 'type-check-exp "invalid index ~a" i))
          (let ([t (list-ref ts i)])
            (values (Prim 'vector-ref (list e^ (Int i)))  t))]
         [else (error 'type-check-exp
                      "expected a vector in vector-ref, not ~a" t)])]
      [(Prim 'vector-set! (list e (Int i) arg) )
       (define-values (e-vec t-vec) (recur e))
       (define-values (e-arg^ t-arg) (recur arg))
       (match t-vec
         [`(Vector ,ts ...)
          (unless (and (exact-nonnegative-integer? i)
                       (i . < . (length ts)))
            (error 'type-check-exp "invalid index ~a" i))
          (unless (type-equal? (list-ref ts i) t-arg)
            (error 'type-check-exp "type mismatch in vector-set! ~a ~a" 
                   (list-ref ts i) t-arg))
          (values (Prim 'vector-set! (list e-vec (Int i) e-arg^))  'Void)]
         [else (error 'type-check-exp
                      "expected a vector in vector-set!, not ~a"
                      t-vec)])]
      [(Prim 'eq? (list arg1 arg2))
       (define-values (e1 t1) (recur arg1))
       (define-values (e2 t2) (recur arg2))
       (match* (t1 t2)
         [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
          ;; allow comparison of vectors of different element types
          (void)]
         [(other wise)
          (unless (type-equal? t1 t2)
            (error 'type-check-exp
                   "type error: different argument types of eq?: ~a != ~a"
                   t1 t2))])
       (values (Prim 'eq? (list e1 e2)) 'Boolean)]
      [(Prim op es)
       (define-values (new-es ts)
         (for/lists (new-es ts) ([e es])
           (recur e)))
       (define t-ret (type-check-op op ts))
       (values (Prim op new-es) t-ret)]
      [(HasType (Prim 'vector es) t)
       ((type-check-exp env) (Prim 'vector es))]
      [(HasType e t)
       (define-values (e^ t^) (recur e))
       (unless (type-equal? t t^)
         (error 'type-check-exp "type mismatch in HasType" t t^))
       (values (HasType e^ t) t)]
      [(GlobalValue name)
       (values (GlobalValue name) 'Integer)]
      [(Allocate size t)
       (values (Allocate size t) t)]
      [(Collect size)
       (values (Collect size) 'Void)]
      [else 
       (error 'type-check-exp "R3/unmatched ~a" e)]
      ))
    )

(define (type-check-R3 e)
  (match e
    [(Program info body)
     (define-values (body^ Tb) ((type-check-exp '()) body))
     (unless (type-equal? Tb 'Integer)
       (error 'type-check-R3 "result type of the program must be Integer, not ~a" Tb))
     (Program info body^)]
    [else
     (error 'type-check-R3 "couldn't match ~a" e)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-C2

(define (type-check-stmt env)
  (lambda (s)
    (match s
      [(Assign (Var x) e)
       (define-values (e^ t) ((type-check-exp env) e))
       (cond [(dict-has-key? env x)
              (unless (type-equal? t (dict-ref env x))
                (error 'type-check-stmt
                       "type error: variable and RHS have different types, ~a != ~a"
                       t (dict-ref env x)))]
             [else
              (dict-set! env x t)])]
      [(Collect size)
       (void)]
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
         (error 'type-check-tail
                "type error: condition must be Boolean, not ~a" Tc))
       (define T1 ((type-check-tail env block-env G) tail1))
       (define T2 ((type-check-tail env block-env G) tail2))
       (unless (type-equal? T1 T2)
         (error 'type-check-tail
                "type error: branches of if should have same type, but ~a != ~a"
                T1 T2))
       T1]
      )))

(define (type-check-C2 p)
  (match p
    [(Program info (CFG G))
     ;; Top-down traversal so we see variable definitions before uses.
     ;; -Jeremy
     (define env (make-hash))
     (define block-env (make-hash))
     (define t ((type-check-tail env block-env G)
                (dict-ref G 'start)))
     (unless (type-equal? t 'Integer)
       (error 'type-check-C2 "return type of program must be Integer, not ~a" t))
     (define locals-types (for/list ([(x t) (in-dict env)])
                                 (cons x t)))
     (define new-info (dict-set info 'locals-types locals-types))
     (Program new-info (CFG G))]))
