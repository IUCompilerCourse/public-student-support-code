#lang racket
(require "utilities.rkt")
(require (only-in "type-check-R2.rkt" operator-types))
(provide type-check-R5 type-check-C4 typed-vars type-equal? type-check-apply
         fun-def-type type-check-def)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-R5

;; lenient type checking for '_
(define (type-equal? t1 t2)
  (match (list t1 t2)
    [(list '_ t2) #t]
    [(list t1 '_) #t]
    [(list `(Vector ,ts1 ...)
           `(Vector ,ts2 ...))
     (for/and ([t1 ts1] [t2 ts2])
       (type-equal? t1 t2))]
    [(list `(,ts1 ... -> ,rt1)
           `(,ts2 ... -> ,rt2))
     (and (for/and ([t1 ts1] [t2 ts2])
            (type-equal? t1 t2))
          (type-equal? rt1 rt2))]
    [else
     (equal? t1 t2)]))

;; type-check-op is copy and pasted from type-check-R2
;; to get updated type-equal?. 
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

(define (type-check-apply env e es)
  (define-values (e^ ty) ((type-check-exp env) e))
  (define-values (e* ty*) (for/lists (e* ty*) ([e (in-list es)])
                            ((type-check-exp env) e)))
  (match ty
    [`(,ty^* ... -> ,rt)
     (for ([arg-ty ty*] [param-ty ty^*])
       (unless (type-equal? arg-ty param-ty)
         (error "type error: argument/parameter mismatch: ~a != ~a"
                arg-ty param-ty e es)))
     (values e^ e* rt)]
    [else (error "type error: expected a function, not" ty)]))

(define typed-vars (make-parameter #f))

(define (type-check-exp env)
  (lambda (e)
    (define recur (type-check-exp env))
    (match e
      [(HasType (Var x) t)
       ((type-check-exp env) (Var x))]
      [(Var x)
       (define t (dict-ref env x))
       (define var (cond [(typed-vars) (HasType (Var x) t)]
                         [else (Var x)]))
       (values var t)]
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
         (error "branches of if must have the same type, but are not"
                (list Tt Te)))
       (values (If c t e) Te)]
      [(Prim 'eq? (list e1 e2))
       (define-values (e1^ T1) (recur e1))
       (define-values (e2^ T2) (recur e2))
       (unless (type-equal? T1 T2)
         (error "arguments of eq? must have the same type, but are not"
                (list T1 T2)))
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
            (values
             (Prim 'vector-ref (list e^ (Int i)))
             t))]
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
          (values (Prim 'vector-set! (list e-vec (Int i) e-arg^))
                  'Void)]
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
      [(FunRef f)
       (let ([t (dict-ref env f)])
         (values (FunRef f) t))]
      [(Apply e es)
       (define-values (e^ es^ rt) (type-check-apply env e es))
       (values (Apply e^ es^) rt)]
      [(Call e es)
       (define-values (e^ es^ rt) (type-check-apply env e es))
       (values (Call e^ es^) rt)]
      [(Lambda (and bnd `([,xs : ,Ts] ...)) rT body)
       (define-values (new-body bodyT) 
         ((type-check-exp (append (map cons xs Ts) env)) body))
       (define ty `(,@Ts -> ,rT))
       (cond
        [(type-equal? rT bodyT)
         (values (Lambda bnd rT new-body) ty)]
        [else
         (error "function body's type does not match return type" bodyT rT)
         ])]
      [else 
       (error 'type-check-exp "R5/unmatched ~a" e)]
      ))
    )

(define (type-check-def env)
  (lambda (e)
    (match e
      [(Def f (and p:t* (list `[,xs : ,ps] ...)) rt info body)
       (define new-env (append (map cons xs ps) env))
       (define-values (body^ ty^) ((type-check-exp new-env) body))
       (unless (type-equal? ty^ rt)
         (error "body type ~a and return type ~a mismatch for ~a"
                ty^ rt e))
       (Def f p:t* rt info body^)]
      [else (error 'type-check-def "ill-formed function definition ~a" e)]
      )))	 

(define (fun-def-type d)
  (match d
    [(Def f (list `[,xs : ,ps] ...) rt info body)  `(,@ps -> ,rt)]
    [else (error 'fun-def-type "ill-formed function definition in ~a" d)]))

(define (type-check-R5 e)
  (match e
    [(ProgramDefs info ds)
     (define new-env (for/list ([d ds]) 
                       (cons (Def-name d) (fun-def-type d))))
     (define ds^ (for/list ([d ds])
                   ((type-check-def new-env) d)))
     ;; TODO: check that main has Integer return type.
     (ProgramDefs info ds^)]
    [(ProgramDefsExp info ds body)
     (define new-env (for/list ([d ds]) 
                       (cons (Def-name d) (fun-def-type d))))
     (define ds^ (for/list ([d ds])
                   ((type-check-def new-env) d)))
     (define-values (body^ ty) ((type-check-exp new-env) body))
     (unless (type-equal? ty 'Integer)
       (error "result of the program must be an integer, not " ty))
     (ProgramDefsExp info ds^ body^)]
    [(Program info body)
     (define-values (body^ ty) ((type-check-exp '()) body))
     (unless (type-equal? ty 'Integer)
       (error "result of the program must be an integer, not " ty))
     (define result (ProgramDefsExp info '() body^))
     result]
    [else
     (error 'type-check-R5 "couldn't match" e)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-C4

(define (type-check-stmt env)
  (lambda (s)
    (match s
      [(Assign (Var x) e)
       (define-values (e^ t) ((type-check-exp env) e))
       (cond [(dict-has-key? env x)
              (unless (type-equal? t (dict-ref env x))
                (error 'type-check-stmt
                       "type error: variable and RHS have different types"
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
         (error "type error: condition of if should be Boolean, not" Tc))
       (define T1 ((type-check-tail env block-env G) tail1))
       (define T2 ((type-check-tail env block-env G) tail2))
       (unless (type-equal? T1 T2)
         (error "type error: branches of if should have same type, not"
                T1 T2))
       T1]
      [(TailCall f arg*)
       (define-values (f^ arg*^ rt) (type-check-apply env f arg*))
       rt]
      )))

(define (type-check-C-def global-env)
  (lambda (d)
    (match d
      [(Def f (and p:t* (list `[,xs : ,ps] ...)) rt info CFG)
       (define new-env (append (map cons xs ps) global-env))
       (define env (make-hash new-env))
       (define block-env (make-hash))
       (define t ((type-check-tail env block-env CFG)
                  (dict-ref CFG (symbol-append f 'start))))
       (unless (type-equal? t rt)
         (error "mismatching return type" t rt))
       (define locals-types
         (for/list ([(x t) (in-dict env)]
                    #:when (not (dict-has-key? global-env x)))
           (cons x t)))
       (define new-info (dict-set info 'locals-types locals-types))
       (Def f p:t* rt new-info CFG)]
      )))

(define (type-check-C4 p)
  (match p
    [(ProgramDefs info ds)
     (define new-env (for/list ([d ds]) 
                       (cons (Def-name d) (fun-def-type d))))
     (define ds^ (for/list ([d ds])
                   ((type-check-C-def new-env) d)))
     (ProgramDefs info ds^)]))

