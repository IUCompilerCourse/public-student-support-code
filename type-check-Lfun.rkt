#lang racket
(require "utilities.rkt")
(require "type-check-Lvec.rkt")
(require "type-check-Lvecof.rkt")
(provide type-check-Lfun type-check-Lfun-has-type
         type-check-Lfun-class type-check-fun-mixin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Functions                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-fun-mixin (for use in Lfun and Cfun)

(define (type-check-fun-mixin super-class)
  (class super-class
    (super-new)
    (inherit check-type-equal?)
    
    (define/override (type-equal? t1 t2)
      (match* (t1 t2)
        [(`(,ts1 ... -> ,rt1) `(,ts2 ... -> ,rt2))
         (and (for/and ([t1 ts1] [t2 ts2])
                (type-equal? t1 t2))
              (type-equal? rt1 rt2))]
        [(other wise) (super type-equal? t1 t2)]))
    
    (define/public (type-check-apply env e es)
      (define-values (e^ ty) ((type-check-exp env) e))
      (define-values (e* ty*) (for/lists (e* ty*) ([e (in-list es)])
                                ((type-check-exp env) e)))
      (match ty
        [`(,ty^* ... -> ,rt)
         (for ([arg-ty ty*] [param-ty ty^*])
           (check-type-equal? arg-ty param-ty (Apply e es)))
         (values e^ e* rt)]
        [else (error 'type-check "expected a function, not ~a" ty)]))

    (define/public (fun-def-type d)
      (match d [(Def f (list `[,xs : ,ps] ...) rt info body)  `(,@ps -> ,rt)]
        [else (error 'type-check "ill-formed function definition in ~a" d)]))

    (define/override (type-check-exp env)
      (lambda (e)
        (match e
          [(FunRef f n)
           (values (FunRef f n)  (dict-ref env f))]
          [(Call e es)
           (define-values (e^ es^ rt) (type-check-apply env e es))
           (values (Call e^ es^) rt)]
          [else ((super type-check-exp env) e)]
          )))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Lfun

;; TODO: Don't allow eq? on function types. -Jeremy

(define type-check-Lfun-class
  (class (type-check-fun-mixin type-check-Lvecof-class)
    (super-new)
    (inherit check-type-equal? type-check-apply fun-def-type)

    (field [max-parameters 32])

    (define/override (type-check-exp env)
      (lambda (e)
        (match e
          [(Apply e es)
           (define-values (e^ es^ rt) (type-check-apply env e es))
           (values (Apply e^ es^) rt)]
          [else ((super type-check-exp env) e)]
          )))

    (define/public (type-check-def env)
      (lambda (e)
        (match e
          [(Def f (and p:t* (list `[,xs : ,ps] ...)) rt info body)
           (unless (< (length xs) max-parameters)
             (error 'type-check "~a has too many parameters, max is ~a"
                    f max-parameters))
           (define new-env (append (map cons xs ps) env))
           (define-values (body^ ty^) ((type-check-exp new-env) body))
           (check-type-equal? ty^ rt body)
           (Def f p:t* rt info body^)]
          [else (error 'type-check "ill-formed function definition ~a" e)]
          )))	 

    (define/override (type-check-program e)
      (match e
        [(ProgramDefsExp info ds body)
         (define new-env (for/list ([d ds])
                           (cons (Def-name d) (fun-def-type d))))
         (define ds^ (for/list ([d ds]) ((type-check-def new-env) d)))
         (define-values (body^ ty) ((type-check-exp new-env) body))
         (check-type-equal? ty 'Integer body)
         (ProgramDefsExp info ds^ body^)]
        [(ProgramDefs info ds)
         (define new-env (for/list ([d ds]) 
                           (cons (Def-name d) (fun-def-type d))))
         (define ds^ (for/list ([d ds]) ((type-check-def new-env) d)))
         ;; TODO: check that main has Integer return type.
         (ProgramDefs info ds^)]
        [(Program info body)
         (define-values (body^ ty) ((type-check-exp '()) body))
         (check-type-equal? ty 'Integer body)
         (ProgramDefsExp info '() body^)]
        [else (error 'type-check "unrecognized ~a" e)]))
    ))

(define (type-check-Lfun p)
  (send (new type-check-Lfun-class) type-check-program p))

(define (type-check-Lfun-has-type p)
  (begin
    (typed-vec #t)
    (typed-vecof #t)
    (define t (type-check-Lfun p))
    (typed-vec #f)
    (typed-vecof #f)
    t))

