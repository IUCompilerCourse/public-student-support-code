#lang racket
(require graph)
(require "multigraph.rkt")
(require "utilities.rkt")
(require "type-check-Cvar.rkt")
(require "type-check-Lif.rkt")
(provide type-check-Cif type-check-Cif-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cif

(define type-check-Cif-class
  (class (type-check-if-mixin type-check-Cvar-class)
    (super-new)
    (inherit type-check-exp type-equal? check-type-equal? combine-types)

    ;; TODO: move some things from here to later type checkers
    (define/public (free-vars-exp e)
      (define (recur e) (send this free-vars-exp e))
      (match e
        [(Var x) (set x)]
        [(Int n) (set)]
        [(Bool b) (set)]
        [(Let x e body)
	 (set-union (recur e) (set-remove (recur body) x))]
        [(If cnd thn els)
         (set-union (recur cnd) (recur thn) (recur els))]
	[(Prim op es)
	 (apply set-union (cons (set) (map recur es)))]
	[else (error 'free-vars-exp "unmatched ~a" e)]))
    
    (field [type-changed #t])

    (define/public (exp-ready? exp env)
      (for/and ([x (free-vars-exp exp)])
        (dict-has-key? env x)))

    (define/public (update-type x t env)
      (debug 'update-type x t)
      (cond [(dict-has-key? env x)
             (define old-t (dict-ref env x))
             (unless (type-equal? t old-t)
               (error 'update-type "old type ~a and new type ~ are inconsistent"
                      old-t t))
             (define new-t (combine-types old-t t))
             (cond [(not (equal? new-t old-t))
                    (dict-set! env x new-t)
                    (set! type-changed #t)])]
            [(eq? t '_)
             (void)]
            [else
             (set! type-changed #t)
             (dict-set! env x t)]))
    
    (define/override ((type-check-atm env) e)
      (match e
        [(Bool b) (values (Bool b) 'Boolean)]
        [else
         ((super type-check-atm env) e)]
        ))
    
    (define/override (type-check-stmt env)
      (lambda (s)
        (debug 'type-check-stmt "Cwhile" s env)
        (match s
          [(Assign (Var x) e)
           #:when (exp-ready? e env)
           (define-values (e^ t) ((type-check-exp env) e))
           (update-type x t env)]
          [(Assign (Var x) e)
           (debug 'type-check-stmt "RHS not ready" e)
           (void)]
          [(Prim 'read '()) (void)]
          [else (void)]
          )))
    
    (define/override ((type-check-tail env block-env blocks) t)
      (debug 'type-check-tail "Cif" t)
      (match t
        [(Return e)
         #:when (exp-ready? e env)
         (define-values (e^ t) ((type-check-exp env) e))
         t]
        [(Return e) '_]      
        [(Seq s t)
         ((type-check-stmt env) s)
         ((type-check-tail env block-env blocks) t)]
        [(Goto label)
         (cond [(dict-has-key? block-env label)
                (dict-ref block-env label)]
               [else '_])]
        [(IfStmt cnd tail1 tail2)
         (cond [(exp-ready? cnd env)
                (define-values (c Tc) ((type-check-exp env) cnd))
                (unless (type-equal? Tc 'Boolean)
                  (error "type error: condition should be Boolean, not" Tc))
                ])
         (define T1 ((type-check-tail env block-env blocks) tail1))
         (define T2 ((type-check-tail env block-env blocks) tail2))
         (unless (type-equal? T1 T2)
           (error "type error: branches of if should have same type, not"
                  T1 T2))
         (combine-types T1 T2)]
        [else ((super type-check-tail env block-env blocks) t)]))

    (define/public (adjacent-tail t)
      (match t
        [(Goto label) (set label)]
        [(IfStmt cnd t1 t2) (set-union (adjacent-tail t1) (adjacent-tail t2))]
        [(Seq s t) (adjacent-tail t)]
        [else (set)]))

    (define/public (C-blocks->CFG blocks)
      (define G (make-multigraph '()))
      (for ([label (in-dict-keys blocks)])
        (add-vertex! G label))
      (for ([(src b) (in-dict blocks)])
        (for ([tgt (adjacent-tail b)])
          (add-directed-edge! G src tgt)))
      G)

    ;; Do the iterative dataflow analysis because of deadcode
    ;; in the un-optimized version of the compiler. -Jeremy
    (define/public (type-check-blocks info blocks env start)
      (define block-env (make-hash))
      (set! type-changed #t)
      (define (iterate)
        (cond [type-changed
               (set! type-changed #f)
               (for ([(label tail) (in-dict blocks)])
                 (define t ((type-check-tail env block-env blocks) tail))
                 (update-type label t block-env)
                 )
               (verbose "type-check-blocks" env block-env)
               (iterate)]
              [else (void)]))
      (iterate)
      (unless (dict-has-key? block-env start)
        (error 'type-check-blocks "failed to infer type for ~a" start))
      (define t (dict-ref block-env start))
      (values env t))

    (define/override (type-check-program p)
      (match p
        [(CProgram info blocks)
         (define empty-env (make-hash))
         (define-values (env t)
           (type-check-blocks info blocks empty-env 'start))
         (unless (type-equal? t 'Integer)
           (error "return type of program must be Integer, not" t))
         (define locals-types
           (for/list ([(x t) (in-dict env)])
             (cons x t)))
         (define new-info (dict-set info 'locals-types locals-types))
         (CProgram new-info blocks)]
        [else (super type-check-program p)]))
    
    ))

(define (type-check-Cif p)
  (send (new type-check-Cif-class) type-check-program p))
  
