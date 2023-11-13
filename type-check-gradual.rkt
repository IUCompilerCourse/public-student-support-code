#lang racket
(require "utilities.rkt")
(require "type-check-Lany.rkt")
(require "type-check-Cany.rkt")

(provide type-check-gradual type-check-gradual-class type-check-gradual-mixin
         type-check-Lany-proxy type-check-Lany-proxy-class
         type-check-Cany-proxy type-check-Cany-proxy-class
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (type-check-gradual-mixin super-class)
  (class super-class
    (super-new)

    (define/public (join t1 t2)
      (match* (t1 t2)
        [('Integer 'Integer) 'Integer]
        [('Boolean 'Boolean) 'Boolean]
        [('Void  'Void) 'Void]
        [('Any t2) t2]
        [(t1 'Any) t1]
        [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
         `(Vector ,@(for/list ([t1 ts1] [t2 ts2]) (join t1 t2)))]
        [(`(Vectorof ,t1) `(Vectorof ,t2))
         `(Vectorof ,(join t1 t2))]
        [(`(,ts1 ... -> ,rt1) `(,ts2 ... -> ,rt2))
         `(,@(for/list ([t1 ts1] [t2 ts2]) (join t1 t2))
           -> ,(join rt1 rt2))]
	[(other wise) (error 'join "unhandled case types ~a ~a" t1 t2)]
	))

    (define/public (meet t1 t2)
      (match* (t1 t2)
        [('Integer 'Integer) 'Integer]
        [('Boolean  'Boolean) 'Boolean]
        [('Void 'Void) 'Void]
        [('Any t2) 'Any]
        [(t1 'Any) 'Any]
        [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
         `(Vector ,@(for/list ([t1 ts1] [t2 ts2]) (meet t1 t2)))]
        [(`(Vectorof ,t1) `(Vectorof ,t2))
         `(Vectorof ,(meet t1 t2))]
        [(`(,ts1 ... -> ,rt1) `(,ts2 ... -> ,rt2))
         `(,@(for/list ([t1 ts1] [t2 ts2]) (meet t1 t2))
           -> ,(meet rt1 rt2))]
	[(other wise) (error 'meet "unhandled case types ~a ~a" t1 t2)]
	))
     
    ))

(define type-check-gradual-class
  (class (type-check-gradual-mixin type-check-Lany-class)
    (super-new)
    (inherit operator-types type-predicates join meet)
    
    (define/override (fun-def-type d)
      (match d
	[(Def f params rt info body)
         (debug 'fun-def-type "parameters:" params)
         (define ps
           (for/list ([p params])
             (match p
               [`[,x : ,T] T]
               [(? symbol?) 'Any]
               [else (error 'fun-def-type "unmatched parameter ~a" p)])))
	 `(,@ps -> ,rt)]
	[else
         (error 'fun-def-type "ill-formed function definition in ~a" d)]))

    (define/public (consistent? t1 t2)
      (match* (t1 t2)
        [('Integer 'Integer) #t]
        [('Boolean 'Boolean) #t]
        [('Void 'Void) #t]
        [('Any t2) #t]
        [(t1 'Any) #t]
        [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
         (for/and ([t1 ts1] [t2 ts2]) (consistent? t1 t2))]
        [(`(Vectorof ,t1) `(Vectorof ,t2))
         (consistent? t1 t2)]
        [(`(,ts1 ... -> ,rt1) `(,ts2 ... -> ,rt2))
         (and (for/and ([t1 ts1] [t2 ts2]) (consistent? t1 t2))
              (consistent? rt1 rt2))]
        [(other wise) #f]))

    (define/public (check-consistent? t1 t2 e)
      (unless (consistent? t1 t2)
        (error 'type-check "~a is inconsistent with ~a\nin ~v" t1 t2 e)))

    ;; Override type-check-op to check for consistency instead of equality.
    (define/override (type-check-op op arg-types e)
      (match (dict-ref (operator-types) op)
        [`(,param-types . ,return-type)
         (for ([at arg-types] [pt param-types]) 
           (check-consistent? at pt e))
         return-type]
        [else (error 'type-check-op "unrecognized ~a" op)]))

    ;; These primitive operators are handled explicitly in the
    ;; type checkers, so don't use type-check-op on them.
    (define explicit-prim-ops
      (set-union
       (type-predicates)
       (set 'procedure-arity 'eq? 'and 'or
            'vector 'vector-length 'vector-ref 'vector-set!
            'make-vector
            'any-vector-length 'any-vector-ref 'any-vector-set!)))

    (define/override (type-check-exp env)
      (lambda (e)
        (verbose "gradual/type-check-exp" e)
	(define recur (type-check-exp env))
	(match e
          ;; Lvar
	  [(Prim op es)
           #:when (not (set-member? explicit-prim-ops op))
           (define-values (new-es ts)
             (for/lists (exprs types) ([e es])
               (recur e)))
           (define t-ret (type-check-op op ts e))
           (values (Prim op new-es) t-ret)]
          ;; Lif
          [(Prim 'eq? (list e1 e2))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (check-consistent? t1 t2 e)
           (define T (meet t1 t2))
           (values (Prim 'eq? (list e1^ e2^)) 'Boolean)]
          [(Prim 'and (list e1 e2))
           (recur (If e1 e2 (Bool #f)))]
          [(Prim 'or (list e1 e2))
           (define tmp (gensym 'tmp))
           (recur (Let tmp e1 (If (Var tmp) (Var tmp) e2)))]
          [(If e1 e2 e3)
           (define-values (e1^ T1) (recur e1))
	   (define-values (e2^ T2) (recur e2))
	   (define-values (e3^ T3) (recur e3))
           (check-consistent? T1 'Boolean e)
	   (check-consistent? T2 T3 e)
           (define Tif (meet T2 T3))
           (values (If e1^ e2^ e3^) Tif)]
          ;; Lwhile
          [(SetBang x e1)
           (define-values (e1^ T1) (recur e1))
           (define varT (dict-ref env x))
           (check-consistent? T1 varT e)
           (values (SetBang x e1^) 'Void)]
          [(WhileLoop e1 e2)
           (define-values (e1^ T1) (recur e1))
           (check-consistent? T1 'Boolean e)
           (define-values (e2^ T2) ((type-check-exp env) e2))
           (values (WhileLoop e1^ e2^) 'Void)]
          ;; Lvec
	  [(Prim 'vector-length (list e1))
           (define-values (e1^ t) (recur e1))
	   (match t
             [`(Vector ,ts ...)
              (values (Prim 'vector-length (list e1^)) 'Integer)]
             ['Any (values (Prim 'vector-length (list e1^)) 'Integer)]
	     [`(Vectorof ,elt-type)
	      (error 'type-check "unhandled Vectorof in vector-length")]
	     )]
	  [(Prim 'vector-ref (list e1 e2))
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (check-consistent? t2 'Integer e)
	   (match t1
	     [`(Vector ,ts ...)
              (match e2^
                [(Int i)
                 (unless (and (0 . <= . i) (i . < . (length ts)))
                   (error 'type-check "invalid index ~a in ~a" i e))
                 (values (Prim 'vector-ref (list e1^ (Int i))) (list-ref ts i))]
                [else (values (Prim 'vector-ref (list e1^ e2^)) 'Any)])]
             [`(Vectorof ,elt-type)
              (values (Prim 'vector-ref (list e1^ e2^)) elt-type)]
             ['Any
              (values (Prim 'vector-ref (list e1^ e2^)) 'Any)]
             [else (error 'type-check "expected vector not ~a\nin ~v" t1 e)])]
	  [(Prim 'vector-set! (list e1 e2 e3) )
           (define-values (e1^ t1) (recur e1))
           (define-values (e2^ t2) (recur e2))
           (define-values (e3^ t3) (recur e3))
           (check-consistent? t2 'Integer e)
	   (match t1
	     [`(Vector ,ts ...)
              (match e2^
                [(Int i)
                 (unless (and (0 . <= . i) (i . < . (length ts)))
                   (error 'type-check "invalid index ~a in ~a" i e))
                 (check-consistent? (list-ref ts i) t3 e)
                 (values (Prim 'vector-set! (list e1^ (Int i) e3^)) 'Void)]
                [else
                 (values (Prim 'vector-set! (list e1^ e2^ e3^)) 'Void)])]
             [`(Vectorof ,elt-type)
              (values (Prim 'vector-set! (list e1^ e2^ e3^))  'Void)]
             ['Any
              (values (Prim 'vector-set! (list e1^ e2^ e3^)) 'Void)]
	     [else (error 'type-check "expected vector not ~a\nin ~v" t1 e)])]
          ;; Llambda
	  [(Apply e1 e2s)
	   (define-values (e1^ T1) (recur e1))
	   (define-values (e2s^ T2s) (for/lists (e* ty*) ([e2 e2s]) (recur e2)))
	   (match T1
	     [`(,T1ps ... -> ,T1rt)
              (for ([T2 T2s] [Tp T1ps])
                (check-consistent? T2 Tp e))
	      (values (Apply e1^ e2s^) T1rt)]
             [`Any
              (values (Apply e1^ e2s^) 'Any)]
	     [else (error 'type-check "expected function not ~a\nin ~v" T1 e)])]
          [(Lambda params Tr e1)
           (define-values (xs Ts) (for/lists (l1 l2) ([p params])
                                    (match p
                                      [`[,x : ,T] (values x T)]
                                      [(? symbol? x) (values x 'Any)])))
           (define-values (e1^ T1) 
             ((type-check-exp (append (map cons xs Ts) env)) e1))
           (check-consistent? Tr T1 e)
           (values (Lambda (for/list ([x xs] [T Ts]) `[,x : ,T]) Tr e1^)
                   `(,@Ts -> ,Tr))]
          [else  ((super type-check-exp env) e)]
          )))

    (define/override (type-check-def env)
      (lambda (e)
	(match e
	  [(Def f params rt info body)
           (define-values (xs ps) (for/lists (l1 l2) ([p params])
                                    (match p
                                      [`[,x : ,T] (values x T)]
                                      [(? symbol? x) (values x 'Any)])))
	   (define new-env (append (map cons xs ps) env))
	   (define-values (body^ ty^) ((type-check-exp new-env) body))
	   (check-consistent? ty^ rt e)
	   (Def f (for/list ([x xs] [T ps]) `[,x : ,T]) rt info body^)]
	  [else (error 'type-check "ill-formed function definition ~a" e)]
	  )))
    
    (define/override (type-check-program e)
      (match e
        [(Program info body)
         (define-values (body^ ty) ((type-check-exp '()) body))
         (check-consistent? ty 'Integer e)
         (ProgramDefsExp info '() body^)]
        [(ProgramDefsExp info ds body)
         (define new-env (for/list ([d ds]) 
                           (cons (Def-name d) (fun-def-type d))))
         (define ds^ (for/list ([d ds])
                       ((type-check-def new-env) d)))
         (define-values (body^ ty) ((type-check-exp new-env) body))
         (check-consistent? ty 'Integer e)
         (ProgramDefsExp info ds^ body^)]
        [else (super type-check-program e)]))
    
    ))

(define (type-check-gradual p)
  (send (new type-check-gradual-class) type-check-program p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Lany-proxy

(define (type-check-Lany-proxy-mixin super-class)
  (class super-class
    (super-new)
    (inherit check-type-equal?)

    (define/override (flat-ty? ty)
      (match ty
        [`(PVector ,ts ...)
         (for/and ([t ts]) (eq? t 'Any))]
        [else (super flat-ty? ty)]))

    (define/override (type-equal? t1 t2)
      (match (list t1 t2)
        [(list `(PVector ,ts1 ...) `(PVector ,ts2 ...))
         (for/and ([t1 ts1] [t2 ts2])
           (type-equal? t1 t2))]
        [else (super type-equal? t1 t2)]))
    
    (define/override ((type-check-exp env) e)
      (define recur (type-check-exp env))
      (match e
        [(Prim 'inject-vector (list e1))
         (define-values (e1^ T1) (recur e1))
         (match T1
           [`(Vector ,ts ...)
            (values (Prim 'inject-vector (list e1^)) `(PVector ,@ts))]
           )]
        [(Prim 'inject-proxy (list e1))
         (define-values (e1^ T1) (recur e1))
         (match T1
           [`(Vector (PVector ,ts0 ...) (Vector (,ts1 -> ,ts2) ...) ,ws)
            (values (Prim 'inject-proxy (list e1^)) `(PVector ,@ts2))]
           ;; after closure conversion
           [`(Vector (PVector ,ts0 ...)
                     (Vector (Vector (,clos ,ts1 -> ,ts2)) ...) ,ws)
            (values (Prim 'inject-proxy (list e1^)) `(PVector ,@ts2))]
           )]
        [(Prim 'proxy? (list e1))
         (define-values (e1^ T1) (recur e1))
         (match T1
           [`(PVector ,ts ...)
            (values (Prim 'proxy? (list e1^)) 'Boolean)]
           )]
        [(Prim 'project-vector (list e1))
         (define-values (e1^ T1) (recur e1))
         (match T1
           [`(PVector ,ts ...)
            (values (Prim 'project-vector (list e1^)) `(Vector ,@ts))]
           )]
        [(Prim 'proxy-vector-length (list e1))
         (define-values (e1^ T1) (recur e1))
         (match T1
           [`(PVector ,ts ...)
            (values (Prim 'proxy-vector-length (list e1^)) 'Integer)])]
        [(Prim 'proxy-vector-ref (list e1 e2))
         (define-values (e1^ T1) (recur e1))
         (define-values (e2^ T2) (recur e2))
         (match (list T1 e2^)
           [(list `(PVector ,ts ...) (Int i))
            (unless (and (0 . <= . i) (i . < . (length ts)))
              (error 'type-check "invalid index ~a in ~a" i e))
            (values (Prim 'proxy-vector-ref (list e1^ e2^))
                    (list-ref ts i))])]
        [(Prim 'proxy-vector-set! (list e1 e2 e3))
         (define-values (e1^ T1) (recur e1))
         (define-values (e2^ T2) (recur e2))
         (define-values (e3^ T3) (recur e3))
         (match (list T1 e2^)
           [(list `(PVector ,ts ...) (Int i))
            (unless (and (0 . <= . i) (i . < . (length ts)))
              (error 'type-check "invalid index ~a in ~a" i e))
            (check-type-equal? (list-ref ts i) T3 e)
            (values (Prim 'proxy-vector-set! (list e1^ e2^ e3^))
                    'Void)])]
        [else ((super type-check-exp env) e)]))
    
    ))

(define type-check-Lany-proxy-class
  (type-check-Lany-proxy-mixin type-check-Lany-class))
  
(define (type-check-Lany-proxy p)
  (send (new type-check-Lany-proxy-class) type-check-program p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cany-proxy

(define type-check-Cany-proxy-class
  (type-check-Lany-proxy-mixin type-check-Cany-class))

(define (type-check-Cany-proxy p)
  (send (new type-check-Cany-proxy-class) type-check-program p))

