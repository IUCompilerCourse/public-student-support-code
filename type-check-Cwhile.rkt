#lang racket
(require graph)
(require "multigraph.rkt")
(require "utilities.rkt")
(require (only-in "any.rkt" compile-Rany))
(require (only-in "type-check-Rlambda.rkt" typed-vars))
(require "type-check-Rany.rkt")
(require "type-check-Cany.rkt")
(provide type-check-Cwhile type-check-Cwhile-class)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cwhile

;; Dataflow analysis

(define FV-Rwhile
  (class compile-Rany
    (super-new)
    
    (define/override (free-variables e)
      (define (recur e) (send this free-variables e))
      (match e
        [(Var x)
         (hash x (HasType (Var x) '_))]
        ;; C-level expressions
        [(Void)
         (hash)]
        [(Allocate size ty)
         (hash)]
        [(AllocateClosure len ty arity)
         (hash)]
        [(GlobalValue name)
         (hash)]
        [(Call f arg*)
         (hash-union (recur f) (apply hash-union (map recur arg*)))]
	[else (super free-variables e)]
	))
    ))
(define FV-Rwhile-inst (new FV-Rwhile))

(define (free-vars-exp e)
  (define fvs-hash (send FV-Rwhile-inst free-variables e))
  (list->set (hash-keys fvs-hash)))


(define type-check-Cwhile-class
  (class type-check-Cany-class
    (super-new)
    (inherit type-check-exp check-type-equal? type-equal? combine-types
             type-check-apply fun-def-type)

    (define type-changed #t)

    (define (exp-ready? exp env)
      (for/and ([x (free-vars-exp exp)])
        (dict-has-key? env x)))

    (define (update-type x t env)
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

    (define/override (type-check-stmt env)
      (lambda (s)
        (verbose 'type-check-stmt s)
        (match s
          [(Assign (Var x) e)
           #:when (exp-ready? e env)
           (define-values (e^ t) ((type-check-exp env) e))
           (update-type x t env)]
          [(Assign (Var x) e) (void)]
          [(Collect size) (void)]
          [(Exit) (void)]
          [(Prim 'vector-set! (list vec index rhs))
           #:when (and (exp-ready? vec env) (exp-ready? index env)
                       (exp-ready? rhs env))
           ((type-check-exp env) s)]
          [(Prim 'read '()) (void)]
          [(Call e es)
           #:when (and (exp-ready? e env)
                       (for/and ([arg es]) (exp-ready? arg env)))
           (define-values (e^ es^ rt) (type-check-apply env e es))
           (void)]
          [else (void)]
          )))

    (define/override (type-check-tail env block-env G)
      (lambda (t)
        (verbose 'type-check-tail t)
        (match t
          [(Return e)
           #:when (exp-ready? e env)
           (define-values (e^ t) ((type-check-exp env) e))
           t]
          [(Return e) '_]      
          [(Seq s t)
           ((type-check-stmt env) s)
           ((type-check-tail env block-env G) t)]
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
           (define T1 ((type-check-tail env block-env G) tail1))
           (define T2 ((type-check-tail env block-env G) tail2))
           (unless (type-equal? T1 T2)
             (error "type error: branches of if should have same type, not"
                    T1 T2))
           (combine-types T1 T2)]
          [(TailCall f arg*)
           #:when (and (exp-ready? f env)
                       (for/and ([arg arg*]) (exp-ready? arg env)))
           (define-values (f^ arg*^ rt) (type-check-apply env f arg*))
           rt]
          [(TailCall f arg*) '_]
          [(Exit) '_]
          )))

    (define (adjacent-tail t)
      (match t
        [(Goto label) (set label)]
        [(IfStmt cnd t1 t2) (set-union (adjacent-tail t1) (adjacent-tail t2))]
        [(Seq s t) (adjacent-tail t)]
        [else (set)]))

    (define (C-CFG->graph cfg)
      (define G (make-multigraph '()))
      (for ([label (in-dict-keys cfg)])
        (add-vertex! G label))
      (for ([(src b) (in-dict cfg)])
        (for ([tgt (adjacent-tail b)])
          (add-directed-edge! G src tgt)))
      G)

    (define/override (type-check-def global-env)
      (lambda (d)
        (match d
          [(Def f (and p:t* (list `[,xs : ,ps] ...)) rt info CFG)
           (verbose "type-check-def" f)
           (define env (make-hash (append (map cons xs ps) global-env)))
           (define block-env (make-hash))
           (set! type-changed #t)
           (define (iterate)
             (cond [type-changed
                    (set! type-changed #f)
                    (for ([(label tail) (in-dict CFG)])
                      (define t ((type-check-tail env block-env CFG) tail))
                      (update-type label t block-env)
                      )
                    (verbose "type-check-def" env block-env)
                    (iterate)]
                   [else (void)]))
           (iterate)
           (define start (symbol-append f 'start))
           (unless (dict-has-key? block-env start)
             (error 'type-check-def "failed to infer type for ~a" start))
           (define t (dict-ref block-env start))
           (unless (type-equal? t rt)
             (error "mismatching return type" t rt))
           (define locals-types
             (for/list ([(x t) (in-dict env)]
                        #:when (not (dict-has-key? global-env x)))
               (cons x t)))
           (define new-info (dict-set info 'locals-types locals-types))
           (Def f p:t* rt new-info CFG)]
          )))

    (define/override (type-check-program p)
      (match p
        [(ProgramDefs info ds)
         (define new-env (for/list ([d ds]) 
                           (cons (Def-name d) (fun-def-type d))))
         (define ds^ (for/list ([d ds])
                       ((type-check-def new-env) d)))
         (ProgramDefs info ds^)]))

    ))

(define (type-check-Cwhile p)
  (send (new type-check-Cwhile-class) type-check-program p))
  
